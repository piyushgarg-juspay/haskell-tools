-- | Utility functions for transforming the GHC AST representation into our own.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.BackendGHC.Utils where

import ApiAnnotation (AnnKeywordId)
import Avail (availNamesWithSelectors, availNames, availName)
import BasicTypes (StringLiteral(..))
import DynFlags (xopt)
import FastString (unpackFS, mkFastString)
import FieldLabel as GHC (FieldLbl(..))
import GHC
import HsSyn
import HscTypes
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import Module as GHC
import Name
import Outputable (Outputable(..), showSDocUnsafe)
import Packages
import Finder
import SrcLoc
import GHC.Stack hiding (SrcLoc(..))

import Control.Exception (Exception, throw)
import Control.Monad.Reader
import Control.Reference ((^.), (&))
import Data.Char (isSpace)
import Data.Data (Data(..))
import Data.Function hiding ((&))
import Data.IORef (readIORef)
import Data.List
import Data.Maybe
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.SemaInfoTypes as Sema
import Language.Haskell.Tools.BackendGHC.GHCUtils
import Language.Haskell.Tools.BackendGHC.Monad
import Language.Haskell.Tools.BackendGHC.SourceMap

import Debug.Trace (trace, traceShowId)

createModuleInfo :: ModSummary -> SrcSpan -> [LImportDecl n] -> Trf (Sema.ModuleInfo GhcRn)
createModuleInfo mod nameLoc (filter (not . ideclImplicit . unLoc) -> imports) = do
  let prelude = (xopt ImplicitPrelude $ ms_hspp_opts mod)
                  && all (\idecl -> ("Prelude" /= (GHC.moduleNameString $ unLoc $ ideclName $ unLoc idecl))
                                      || nameLoc == getLoc idecl) imports
  (_, preludeImports) <- if prelude then getImportedNames "Prelude" Nothing else return (ms_mod mod, [])
  deps <- if prelude then lift $ getDeps (Module baseUnitId (GHC.mkModuleName "Prelude"))
                     else return []
  -- This function (via getInstances) refers the ghc environment,
  -- we must evaluate the result or the reference may be kept preventing garbage collection.
  return $ mkModuleInfo (ms_mod mod) (ms_hspp_opts mod) (case ms_hsc_src mod of HsSrcFile -> False; _ -> True)
                        (forceElements preludeImports) deps

-- | Creates a semantic information for a name
createNameInfo :: IdP n -> Trf (NameInfo n)
createNameInfo name = do locals <- asks localsInScope
                         isDefining <- asks defining
                         return (mkNameInfo locals isDefining name)

-- | Creates a semantic information for an ambiguous name (caused by field disambiguation for example)
createAmbigousNameInfo :: RdrName -> SrcSpan -> Trf (NameInfo n)
createAmbigousNameInfo name span = do locals <- asks localsInScope
                                      isDefining <- asks defining
                                      return (mkAmbiguousNameInfo locals isDefining name span)

-- | Creates a semantic information for an implicit name
createImplicitNameInfo :: String -> Trf (NameInfo n)
createImplicitNameInfo name = do locals <- asks localsInScope
                                 isDefining <- asks defining
                                 rng <- asks contRange
                                 return (mkImplicitNameInfo locals isDefining name rng)

-- | Creates a semantic information for an implicit name
createImplicitFldInfo :: (GHCName n, HsHasName (IdP n)) => (a -> IdP n) -> [HsRecField n a] -> Trf ImplicitFieldInfo
createImplicitFldInfo select flds = return (mkImplicitFieldInfo (map getLabelAndExpr flds))
  where getLabelAndExpr fld = ( getTheName $ unLoc (getFieldOccName (hsRecFieldLbl fld))
                              , getTheName $ select (hsRecFieldArg fld) )
        getTheName = (\case e:_ -> e; [] -> convProblem "createImplicitFldInfo: missing names") . hsGetNames'

-- | Adds semantic information to an impord declaration. See ImportInfo.
createImportData :: forall r n . (GHCName r, HsHasName (IdP n)) => GHC.ImportDecl n -> Trf (ImportInfo r)
createImportData (GHC.ImportDecl _ _ name pkg _ _ _ _ _ declHiding) =
  do (mod,importedNames) <- getImportedNames (GHC.moduleNameString $ unLoc name) (fmap (unpackFS . sl_fs) pkg)
     names <- liftGhc $ filterM (checkImportVisible declHiding . (^. pName)) importedNames
     -- TODO: only use getFromNameUsing once
     lookedUpNames <- liftGhc $ mapM translatePName $ names
     lookedUpImported <- liftGhc $ mapM ((getFromNameUsing @r) getTopLevelId . (^. pName)) $ importedNames
     deps <- lift $ getDeps mod
     -- This function (via getInstances) refers the ghc environment,
     -- we must evaluate the result or the reference may be kept preventing garbage collection.
     return $ mkImportInfo mod (forceElements $ catMaybes lookedUpImported)
                               (forceElements $ catMaybes lookedUpNames)
                               deps
  where translatePName :: PName GhcRn -> Ghc (Maybe (PName r))
        translatePName (PName n p) = do n' <- (getFromNameUsing @r) getTopLevelId n
                                        p' <- maybe (return Nothing) ((getFromNameUsing @r) getTopLevelId) p
                                        return (PName <$> n' <*> Just p')

getDeps :: Module -> Ghc [Module]
getDeps mod = do
  env <- GHC.getSession
  eps <- liftIO $ hscEPS env
  case lookupIfaceByModule (hsc_dflags env) (hsc_HPT env) (eps_PIT eps) mod of
    Just ifc -> (mod :) <$> mapM (liftIO . getModule env . fst) (dep_mods (mi_deps ifc))
    Nothing -> return [mod]
  where getModule env modName = do
          res <- findHomeModule env modName
          case res of Found _ m -> return m
                      _ -> case lookupPluginModuleWithSuggestions (hsc_dflags env) modName Nothing of
                             LookupFound m _ -> return m
                             LookupHidden hiddenPack hiddenMod -> return (head $ map fst hiddenMod ++ map fst hiddenPack)
                             _ -> error $ "getDeps: module not found: " ++ GHC.moduleNameString modName

-- | Get names that are imported from a given import
getImportedNames :: String -> Maybe String -> Trf (GHC.Module, [PName GhcRn])
getImportedNames name pkg = liftGhc $ do
  hpt <- hsc_HPT <$> getSession
  eps <- getSession >>= liftIO . readIORef . hsc_EPS
  mod <- findModule (mkModuleName name) (fmap mkFastString pkg)
  -- load exported names from interface file
  let ifaceNames = maybe [] mi_exports $ flip lookupModuleEnv mod
                                       $ eps_PIT eps
  let homeExports = maybe [] (md_exports . hm_details) (lookupHptByModule hpt mod)
  -- TODO: Why selectors are added in one case and not added in the other?
  return (mod, concatMap (availToPName availNames) ifaceNames ++ concatMap (availToPName availNamesWithSelectors) homeExports)
    where availToPName f a = map (\n -> if n == availName a then PName n Nothing else PName n (Just (availName a))) (f a)

-- | Check is a given name is imported from an import with given import specification.
checkImportVisible :: (HsHasName (IdP name), GhcMonad m)
                   => Maybe (Bool, Located [LIE name]) -> GHC.Name -> m Bool
checkImportVisible (Just (isHiding, specs)) name
  | isHiding  = not . or @[] <$> mapM (`ieSpecMatches` name) (map unLoc (unLoc specs))
  | otherwise = or @[] <$> mapM (`ieSpecMatches` name) (map unLoc (unLoc specs))
checkImportVisible _ _ = return True

-- | Forces strict evaluation of a list. Forces elements into WHNF.
forceElements :: [a] -> [a]
forceElements [] = []
forceElements (a : ls) = let res = forceElements ls
                          in a `seq` res `seq` (a : ls)

ieSpecMatches :: (HsHasName (IdP name), GhcMonad m) => IE name -> GHC.Name -> m Bool
ieSpecMatches (concatMap hsGetNames' . HsSyn.ieNames -> ls) name
  | name `elem` ls = return True
-- ieNames does not consider field names
ieSpecMatches (IEThingWith _ thing _ with flds) name
  | name `elem` concatMap hsGetNames' (map (ieWrappedName . unLoc) (thing : with) ++ map (flSelector . unLoc) flds)
  = return True
ieSpecMatches ie@(IEThingAll {}) name | [n] <- hsGetNames' (HsSyn.ieName ie), isTyConName n
  = do entity <- lookupName n
       return $ case entity of Just (ATyCon tc)
                                 | Just cls <- tyConClass_maybe tc
                                     -> name `elem` map getName (classMethods cls)
                                 | otherwise -> name `elem` concatMap (\dc -> getName dc : map flSelector (dataConFieldLabels dc))
                                                                      (tyConDataCons tc)
                               _             -> False
ieSpecMatches _ _ = return False

noSemaInfo :: src -> NodeInfo NoSemanticInfo src
noSemaInfo = NodeInfo mkNoSemanticInfo

-- | Creates a place for a missing node with a default location
nothing :: String -> String -> Trf SrcLoc -> Trf (AnnMaybeG e (Dom n) RangeStage)
nothing bef aft pos = annNothing . noSemaInfo . OptionalPos bef aft <$> pos

emptyList :: String -> Trf SrcLoc -> Trf (AnnListG e (Dom n) RangeStage)
emptyList sep ann = AnnListG <$> (noSemaInfo . ListPos "" "" sep Nothing <$> ann) <*> pure []

-- | Creates a place for a list of nodes with a default place if the list is empty.
makeList :: String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeList sep ann ls = AnnListG <$> (noSemaInfo . ListPos "" "" sep Nothing <$> ann) <*> ls

makeListBefore :: String -> String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeListBefore bef sep ann ls = do isEmpty <- null <$> ls
                                   AnnListG <$> (noSemaInfo . ListPos (if isEmpty then bef else "") "" sep Nothing <$> ann) <*> ls

makeListAfter :: String -> String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeListAfter aft sep ann ls = do isEmpty <- null <$> ls
                                  AnnListG <$> (noSemaInfo . ListPos "" (if isEmpty then aft else "") sep Nothing <$> ann) <*> ls

makeNonemptyList :: String -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeNonemptyList sep ls = AnnListG (noSemaInfo $ ListPos "" "" sep Nothing noSrcLoc) <$> ls

-- | Creates a place for an indented list of nodes with a default place if the list is empty.
makeIndentedList :: Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeIndentedList ann ls = do
  elems <- ls
  indent <- elementsWithoutSemi elems
  AnnListG <$> (noSemaInfo . ListPos  "" "" "\n" (Just indent) <$> ann) <*> pure elems

makeIndentedListNewlineBefore :: Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeIndentedListNewlineBefore ann ls = do elems <- ls
                                          indent <- elementsWithoutSemi elems
                                          AnnListG <$> (noSemaInfo . ListPos (if null elems then "\n" else "") "" "\n" (Just indent) <$> ann) <*> pure elems

makeIndentedListBefore :: String -> Trf SrcLoc -> Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeIndentedListBefore bef sp ls = do elems <- ls
                                      indent <- elementsWithoutSemi elems
                                      AnnListG <$> (noSemaInfo . ListPos (if null elems then bef else "") "" "\n" (Just indent) <$> sp) <*> pure elems

makeNonemptyIndentedList :: Trf [Ann e (Dom n) RangeStage] -> Trf (AnnListG e (Dom n) RangeStage)
makeNonemptyIndentedList ls = do elems <- ls
                                 indent <- elementsWithoutSemi elems
                                 AnnListG (noSemaInfo $ ListPos "" "" "\n" (Just indent) noSrcLoc) <$> pure elems

-- | Get the elements where there is no ; before
elementsWithoutSemi :: [Ann e (Dom n) RangeStage] -> Trf [Bool]
elementsWithoutSemi [] = return []
elementsWithoutSemi (fst:rest) = indentedElements' (srcSpanEnd $ getRange fst) rest
  where indentedElements' lastEnd (elem:rest)
          = let sepRange = mkSrcSpan lastEnd (srcSpanStart $ getRange elem)
             in (:) <$> (not . (\l -> isGoodSrcSpan l && srcSpanStart l < srcSpanEnd l) <$> focusOn sepRange (tokenLoc AnnSemi))
                    <*> indentedElements' (srcSpanEnd $ getRange elem) rest
        indentedElements' _ [] = return []

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfLoc :: (a -> Trf (b (Dom n) RangeStage)) -> Trf (SemanticInfo (Dom n) b) -> Located a -> Trf (Ann b (Dom n) RangeStage)
trfLoc f sema = trfLocCorrect sema pure f

trfLocNoSema :: SemanticInfo (Dom n) b ~ NoSemanticInfo => (a -> Trf (b (Dom n) RangeStage)) -> Located a -> Trf (Ann b (Dom n) RangeStage)
trfLocNoSema f = trfLoc f (pure mkNoSemanticInfo)

-- | Transforms a possibly-missing node with the default location of the end of the focus.
trfMaybe :: String -> String -> (Located a -> Trf (Ann e (Dom n) RangeStage)) -> Maybe (Located a) -> Trf (AnnMaybeG e (Dom n) RangeStage)
trfMaybe bef aft f = trfMaybeDefault bef aft f atTheEnd

-- | Transforms a possibly-missing node with a default location
trfMaybeDefault :: String -> String -> (Located a -> Trf (Ann e (Dom n) RangeStage)) -> Trf SrcLoc -> Maybe (Located a) -> Trf (AnnMaybeG e (Dom n) RangeStage)
trfMaybeDefault _   _   f _   (Just e) = makeJust <$> f e
trfMaybeDefault bef aft _ loc Nothing  = nothing bef aft loc

-- | Transform a located part of the AST by automatically transforming the location
-- with correction by applying the given function. Sets the source range for transforming children.
trfLocCorrect :: Trf (SemanticInfo (Dom n) b) -> (SrcSpan -> Trf SrcSpan) -> (a -> Trf (b (Dom n) RangeStage)) -> Located a -> Trf (Ann b (Dom n) RangeStage)
trfLocCorrect sema locF f (L l e) = annLoc sema (locF l) (f e)

-- | Transform a located part of the AST by automatically transforming the location.
-- Sets the source range for transforming children.
trfMaybeLoc :: (a -> Trf (Maybe (b (Dom n) RangeStage))) -> SemanticInfo (Dom n) b -> Located a -> Trf (Maybe (Ann b (Dom n) RangeStage))
trfMaybeLoc f sema (L l e) = do fmap (Ann (NodeInfo sema (NodeSpan l))) <$> local (\s -> s { contRange = l }) (f e)

trfMaybeLocNoSema :: SemanticInfo (Dom n) b ~ NoSemanticInfo => (a -> Trf (Maybe (b (Dom n) RangeStage))) -> Located a -> Trf (Maybe (Ann b (Dom n) RangeStage))
trfMaybeLocNoSema f = trfMaybeLoc f mkNoSemanticInfo

-- | Creates a place for a list of nodes with the default place at the end of the focus if the list is empty.
trfAnnList :: SemanticInfo (Dom n) b ~ NoSemanticInfo => String -> (a -> Trf (b (Dom n) RangeStage)) -> [Located a] -> Trf (AnnListG b (Dom n) RangeStage)
trfAnnList sep _ [] = makeList sep atTheEnd (pure [])
trfAnnList sep f ls = makeList sep (pure $ noSrcLoc) (mapM (trfLoc f (pure mkNoSemanticInfo)) ls)

trfAnnList' :: String -> (Located a -> Trf (Ann b (Dom n) RangeStage)) -> [Located a] -> Trf (AnnListG b (Dom n) RangeStage)
trfAnnList' sep _ [] = makeList sep atTheEnd (pure [])
trfAnnList' sep f ls = makeList sep (pure $ noSrcLoc) (mapM f ls)


-- | Creates a place for a list of nodes that cannot be empty.
nonemptyAnnList :: [Ann e (Dom n) RangeStage] -> AnnListG e (Dom n) RangeStage
nonemptyAnnList = AnnListG (noSemaInfo $ ListPos "" "" "" Nothing noSrcLoc)

-- | Creates an optional node from an existing element
makeJust :: Ann e (Dom n) RangeStage -> AnnMaybeG e (Dom n) RangeStage
makeJust e = AnnMaybeG (noSemaInfo $ OptionalPos "" "" noSrcLoc) (Just e)

-- | Annotates a node with the given location and focuses on the given source span.
annLoc :: Trf (SemanticInfo (Dom n) b) -> Trf SrcSpan -> Trf (b (Dom n) RangeStage) -> Trf (Ann b (Dom n) RangeStage)
annLoc semam locm nodem = do loc <- locm
                             node <- focusOn loc nodem
                             sema <- semam
                             return (Ann (NodeInfo sema (NodeSpan loc)) node)

annLocNoSema :: SemanticInfo (Dom n) b ~ NoSemanticInfo => Trf SrcSpan -> Trf (b (Dom n) RangeStage) -> Trf (Ann b (Dom n) RangeStage)
annLocNoSema = annLoc (pure mkNoSemanticInfo)

-- * Focus manipulation

focusOn :: SrcSpan -> Trf a -> Trf a
focusOn sp = local (\s -> s { contRange = sp })

updateFocus :: (SrcSpan -> Trf SrcSpan) -> Trf a -> Trf a
updateFocus f trf = do newSpan <- f =<< asks contRange
                       focusOn newSpan trf

focusAfterLoc :: SrcLoc -> Trf a -> Trf a
focusAfterLoc loc = local (\s -> s { contRange = mkSrcSpan loc (srcSpanEnd (contRange s)) })

focusBeforeLoc :: SrcLoc -> Trf a -> Trf a
focusBeforeLoc loc = local (\s -> s { contRange = mkSrcSpan (srcSpanStart (contRange s)) loc })

-- | Focuses the transformation to go between tokens. The tokens must be found inside the current range.
between :: AnnKeywordId -> AnnKeywordId -> Trf a -> Trf a
between firstTok lastTok = focusAfter firstTok . focusBefore lastTok

-- | Focuses the transformation to go between tokens if they are present
betweenIfPresent :: AnnKeywordId -> AnnKeywordId -> Trf a -> Trf a
betweenIfPresent firstTok lastTok = focusAfterIfPresent firstTok . focusBeforeIfPresent lastTok

-- | Focuses the transformation to be performed after the given token. The token must be found inside the current range.
focusAfter :: AnnKeywordId -> Trf a -> Trf a
focusAfter firstTok trf
  = do firstToken <- tokenLoc firstTok
      --  !_ <- trace ("In focus after, token = " ++ show firstTok ++ " , Location = " ++ show firstToken) $ return ()
       if (isGoodSrcSpan firstToken)
          then local (\s -> s { contRange = mkSrcSpan (srcSpanEnd firstToken) (srcSpanEnd (contRange s))}) trf
          else do rng <- asks contRange
                  convertionProblem $ "focusAfter: token not found in " ++ show rng ++ ": " ++ show firstTok

focusAfterIfPresent :: AnnKeywordId -> Trf a -> Trf a
focusAfterIfPresent firstTok trf
  = do firstToken <- tokenLoc firstTok
       if (isGoodSrcSpan firstToken)
          then local (\s -> s { contRange = mkSrcSpan (srcSpanEnd firstToken) (srcSpanEnd (contRange s))}) trf
          else trf

-- | Focuses the transformation to be performed before the given token. The token must be found inside the current range.
focusBefore :: AnnKeywordId -> Trf a -> Trf a
focusBefore lastTok trf
  = do lastToken <- tokenLocBack lastTok
       if (isGoodSrcSpan lastToken)
          then local (\s -> s { contRange = mkSrcSpan (srcSpanStart (contRange s)) (srcSpanStart lastToken)}) trf
          else do rng <- asks contRange
                  convertionProblem $ "focusBefore: token not found in " ++ show rng ++ ": " ++ show lastTok

focusBeforeIfPresent :: AnnKeywordId -> Trf a -> Trf a
focusBeforeIfPresent lastTok trf
  = do lastToken <- tokenLocBack lastTok
       if (isGoodSrcSpan lastToken)
          then local (\s -> s { contRange = mkSrcSpan (srcSpanStart (contRange s)) (srcSpanStart lastToken)}) trf
          else trf

-- | Gets the position before the given token
before :: AnnKeywordId -> Trf SrcLoc
before tok = srcSpanStart <$> tokenLoc tok

-- | Gets the position after the given token
after :: AnnKeywordId -> Trf SrcLoc
after tok = srcSpanEnd <$> tokenLoc tok

-- | The element should span from the given token to the end of focus
annFrom :: AnnKeywordId -> Trf (SemanticInfo (Dom n) e) -> Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annFrom kw sema = annLoc sema (combineSrcSpans <$> tokenLoc kw <*> asks (srcLocSpan . srcSpanEnd . contRange))

annFromNoSema :: SemanticInfo (Dom n) e ~ NoSemanticInfo => AnnKeywordId -> Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annFromNoSema kw = annFrom kw (pure mkNoSemanticInfo)

-- | Gets the position at the beginning of the focus
atTheStart :: Trf SrcLoc
atTheStart = asks (srcSpanStart . contRange)

-- | Gets the position at the end of the focus
atTheEnd :: Trf SrcLoc
atTheEnd = asks (srcSpanEnd . contRange)

-- | Searches for a token inside the focus and retrieves its location
tokenLoc :: AnnKeywordId -> Trf SrcSpan
tokenLoc keyw = do 
  rng <- asks contRange
  fromMaybe noSrcSpan <$> (getKeywordInside keyw <$> asks contRange <*> asks srcMap)

allTokenLoc :: AnnKeywordId -> Trf [SrcSpan]
allTokenLoc keyw = getKeywordsInside keyw <$> asks contRange <*> asks srcMap

-- | Searches for a token backward inside the focus and retrieves its location
tokenLocBack :: AnnKeywordId -> Trf SrcSpan
tokenLocBack keyw = fromMaybe noSrcSpan <$> (getKeywordInsideBack keyw <$> asks contRange <*> asks srcMap)

tokenBefore :: SrcLoc -> AnnKeywordId -> Trf SrcSpan
tokenBefore loc keyw
  = fromMaybe noSrcSpan <$> (getKeywordInsideBack keyw <$> (mkSrcSpan <$> (asks (srcSpanStart . contRange)) <*> pure loc) <*> asks srcMap)

allTokensAfter :: SrcLoc -> Trf [(SrcSpan, AnnKeywordId)]
allTokensAfter loc = getTokensAfter loc <$> asks srcMap

tokensAfter :: AnnKeywordId -> Trf [SrcSpan]
tokensAfter keyw
  = map fst . filter ((==keyw) . snd) <$> (asks (srcSpanEnd . contRange) >>= allTokensAfter)


-- | Searches for tokens in the given order inside the parent element and returns their combined location
tokensLoc :: [AnnKeywordId] -> Trf SrcSpan
tokensLoc keys = foldLocs <$> eachTokenLoc keys

-- | Searches for tokens in the given order inside the parent element and returns their location
eachTokenLoc :: [AnnKeywordId] -> Trf [SrcSpan]
eachTokenLoc (keyw:rest)
  = do spanFirst <- tokenLoc keyw
       spanRest <- focusAfterLoc (srcSpanEnd spanFirst) (eachTokenLoc rest)
       return (spanFirst : spanRest)
eachTokenLoc [] = pure []

-- | Searches for a token and retrieves its location anywhere
uniqueTokenAnywhere :: AnnKeywordId -> Trf SrcSpan
uniqueTokenAnywhere keyw = fromMaybe noSrcSpan <$> (getKeywordAnywhere keyw <$> asks srcMap)

-- | Annotates the given element with the current focus as a location.
annCont :: Trf (SemanticInfo (Dom n) e) -> Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annCont sema = annLoc sema (asks contRange)

annContNoSema :: SemanticInfo (Dom n) e ~ NoSemanticInfo => Trf (e (Dom n) RangeStage) -> Trf (Ann e (Dom n) RangeStage)
annContNoSema = annCont (pure mkNoSemanticInfo)

-- | Annotates the element with the same annotation that is on the other element
copyAnnot :: SemanticInfo (Dom n) a ~ SemanticInfo (Dom n) b
               => (Ann a (Dom n) RangeStage -> b (Dom n) RangeStage) -> Trf (Ann a (Dom n) RangeStage) -> Trf (Ann b (Dom n) RangeStage)
copyAnnot f at = (\(Ann i a) -> Ann i (f (Ann i a))) <$> at

-- | Combine source spans into one that contains them all
foldLocs :: [SrcSpan] -> SrcSpan
foldLocs = foldl combineSrcSpans noSrcSpan

-- | The location after the given string
advanceStr :: String -> SrcLoc -> SrcLoc
advanceStr str (RealSrcLoc l) = RealSrcLoc $ foldl advanceSrcLoc l str
advanceStr _ l = l

-- | Update column information in a source location
updateCol :: (Int -> Int) -> SrcLoc -> SrcLoc
updateCol _ loc@(UnhelpfulLoc _) = loc
updateCol f (RealSrcLoc loc) = mkSrcLoc (srcLocFile loc) (srcLocLine loc) (f $ srcLocCol loc)

-- | Update the start of the src span
updateStart :: (SrcLoc -> SrcLoc) -> SrcSpan -> SrcSpan
updateStart f sp = mkSrcSpan (f (srcSpanStart sp)) (srcSpanEnd sp)

-- | Update the end of the src span
updateEnd :: (SrcLoc -> SrcLoc) -> SrcSpan -> SrcSpan
updateEnd f sp = mkSrcSpan (srcSpanStart sp) (f (srcSpanEnd sp))

-- | Combine source spans of elements into one that contains them all
collectLocs :: [Located e] -> SrcSpan
collectLocs = foldLocs . map getLoc

-- | Rearrange definitions to appear in the order they are defined in the source file.
orderDefs :: [Ann e (Dom n) RangeStage] -> [Ann e (Dom n) RangeStage]
orderDefs = sortBy (compare `on` srcSpanStart . (^. AST.annotation & AST.sourceInfo & AST.nodeSpan))

-- | Orders a list of elements to the order they are defined in the source file.
orderAnnList :: AnnListG e (Dom n) RangeStage -> AnnListG e (Dom n) RangeStage
orderAnnList (AnnListG a ls) = AnnListG a (orderDefs ls)

-- | Only keeps one of the elements that are on the same source location
removeDuplicates :: [Located e] -> [Located e]
removeDuplicates (fst:rest) = fst : removeDuplicates (filter ((/= getLoc fst) . getLoc) rest)
removeDuplicates [] = []

-- | Orders a list of elements to the order they are defined in the source file.
orderLocated :: [Located e] -> [Located e]
orderLocated = sortBy (compare `on` getLoc)

-- | Transform a list of definitions where the defined names are in scope for subsequent definitions
trfScopedSequence :: HsHasName d => (d -> Trf e) -> [d] -> Trf [e]
trfScopedSequence f (def:rest) = (:) <$> f def <*> addToScope def (trfScopedSequence f rest)
trfScopedSequence _ [] = pure []

-- | Splits a given string at whitespaces while calculating the source location of the fragments
splitLocated :: Located String -> [Located String]
splitLocated = splitLocatedOn isSpace

-- | Splits a given string while calculating the source location of the fragments
splitLocatedOn :: (Char -> Bool) -> Located String -> [Located String]
splitLocatedOn pred (L (RealSrcSpan l) str) = splitLocated' str (realSrcSpanStart l) Nothing
  where splitLocated' :: String -> RealSrcLoc -> Maybe (RealSrcLoc, String) -> [Located String]
        splitLocated' (c:rest) currLoc (Just (startLoc, str)) | pred c
          = L (RealSrcSpan $ mkRealSrcSpan startLoc currLoc) (reverse str) : splitLocated' rest (advanceSrcLoc currLoc c) Nothing
        splitLocated' (c:rest) currLoc Nothing | pred c = splitLocated' rest (advanceSrcLoc currLoc c) Nothing
        splitLocated' (c:rest) currLoc (Just (startLoc, str)) = splitLocated' rest (advanceSrcLoc currLoc c) (Just (startLoc, c:str))
        splitLocated' (c:rest) currLoc Nothing = splitLocated' rest (advanceSrcLoc currLoc c) (Just (currLoc, [c]))
        splitLocated' [] currLoc (Just (startLoc, str)) = [L (RealSrcSpan $ mkRealSrcSpan startLoc currLoc) (reverse str)]
        splitLocated' [] _ Nothing = []
splitLocatedOn _ _ = convProblem "splitLocated: unhelpful span given"

compareSpans :: SrcSpan -> SrcSpan -> Ordering
compareSpans (RealSrcSpan a) (RealSrcSpan b)
  | a `containsSpan` b = GT
  | b `containsSpan` a = LT
compareSpans _ _ = EQ

-- | Report errors when cannot convert a type of element
unhandledElement :: (Data a, Outputable a, HasCallStack) => String -> a -> Trf b
unhandledElement label e = convertionProblem ("Illegal " ++ label ++ ": " ++ showSDocUnsafe (ppr e) ++ " (ctor: " ++ show (toConstr e) ++ ")")

unhandledElementNoPpr :: (Data a, HasCallStack) => String -> a -> Trf b
unhandledElementNoPpr label e = convertionProblem ("Illegal " ++ label ++ ": (ctor: " ++ show (toConstr e) ++ ")")

instance Semigroup SrcSpan where
  span1@(RealSrcSpan _) <> _     = span1
  _                     <> span2 = span2

instance Monoid SrcSpan where
  mempty = noSrcSpan

data ConvertionProblem = ConvertionProblem CallStack SrcSpan String
                       | UnrootedConvertionProblem String
  deriving Show

instance Exception ConvertionProblem

convertionProblem :: HasCallStack => String -> Trf a
convertionProblem msg = do rng <- asks contRange
                           throw $ ConvertionProblem callStack rng msg

convProblem :: String -> a
convProblem = throw . UnrootedConvertionProblem
