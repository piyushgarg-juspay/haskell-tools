{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Haskell.Tools.BackendGHC.AddTypeInfo (addTypeInfos) where

import Bag as GHC (bagToList)
import GHC
import HscTypes as GHC
import Id as GHC (Id, mkVanillaGlobal)
import Module as GHC (Module, moduleEnvElts)
import Name as GHC hiding (varName)
import OccName as GHC (OccName, mkDataOcc)
import SrcLoc as GHC
import TcEvidence as GHC (EvBind(..), TcEvBinds(..))
import Type as GHC (Type, mkTyVarTy, mkTyConTy)
import TysWiredIn as GHC (anyTyCon)
import UniqDFM as GHC (eltsUDFM)
import UniqSupply as GHC (uniqFromSupply, mkSplitUniqSupply)
import Var as GHC (Var(..))
import GHC.Hs.Extension

import Control.Applicative (Applicative(..), (<$>), Alternative(..))
import Control.Exception (evaluate, throw)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Reference (Lens_1(..), (^.))
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Operations (universeBi)
import Data.List as List
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (Maybe(..), fromMaybe, catMaybes)
import GHC.Stack (emptyCallStack)

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.SemaInfoTypes as AST
import Language.Haskell.Tools.BackendGHC.GHCUtils (getTopLevelId)
import Language.Haskell.Tools.BackendGHC.Utils (ConvertionProblem(..), forceElements, convProblem)

addTypeInfos :: LHsBinds GhcTc -> Ann AST.UModule (Dom GhcRn) RangeStage -> Ghc (Ann AST.UModule IdDom RangeStage)
addTypeInfos bnds mod = do
  ut <- liftIO mkUnknownType
  let getType = getType' ut
  fixities <- getFixities
  -- evaluates fixities to normal form before going on, otherwise the reference to session would be kept
  liftIO $ evaluate (forceElements fixities)
  let createCName sc def id = mkCNameInfo sc def id fixity
        where fixity = if any (any ((getOccName id ==) . getOccName . (^. _1))) (drop 1 sc)
                          then Nothing
                          else fmap (snd . snd) $ List.find (\(mod,(occ,_)) -> Just mod == (nameModule_maybe $ varName id) && occ == getOccName id) fixities
  evalStateT (semaTraverse
    (AST.SemaTrf
      (\ni -> case (AST.semanticsSourceInfo ni, AST.semanticsName ni) of
                (_, Just name) -> lift $ createCName (AST.semanticsScope ni) (AST.semanticsDefining ni) <$> getType name
                (Just l@(RealSrcSpan loc), _)
                  -> case Map.lookup l locMapping of
                            Just id -> return $ createCName (AST.semanticsScope ni) (AST.semanticsDefining ni) id
                            _ -> do (none,rest) <- gets (break ((\case (RealSrcSpan sp) -> sp `containsSpan` loc) . fst))
                                    case rest of [] -> throw $ ConvertionProblem emptyCallStack (RealSrcSpan loc) "Ambiguous or implicit name missing"
                                                 ((_,id):more) -> do put (none ++ more)
                                                                     return $ createCName (AST.semanticsScope ni) (AST.semanticsDefining ni) id
                _ -> convProblem "addTypeInfos: Cannot access a the semantics of a name.")
      pure fetchLitType (lift . trfImportInfoM getType) (lift . trfModuleInfoM getType) pure
        pure) mod) (extractSigIds bnds ++ extractSigBindIds bnds)
  where locMapping = Map.fromList $ map (\(L l id) -> (l, id)) $ extractExprIds bnds
        getType' :: Type -> Name -> Ghc Id
        getType' ut name = fromMaybe (mkVanillaGlobal name ut) <$> ((<|> Map.lookup name ids) <$> getTopLevelId name)
        ids = Map.fromList $ map (\id -> (getName id, id)) $ extractTypes bnds

        extractTypes :: LHsBinds GhcTc -> [Id]
        extractTypes = concatMap universeBi . bagToList

        fetchLitType :: Monad m => PreLiteralInfo -> m LiteralInfo
        fetchLitType (RealLiteralInfo t) = return $ LiteralInfo t
        fetchLitType (PreLiteralInfo sp) = return $ LiteralInfo $ fromMaybe (convProblem $ "cannot lookup type of literal at: " ++ shortShowSpanWithFile sp) $ lookup sp decompedOverloadedLits

        -- NOTE: Only overloaded literals' type information is collected here
        --       Might not be complete
        --       Collecting only HsOverLits doesn't work,
        --       because they contain only UnhelpfulSrcSpans
        decompedOverloadedLits :: [(SrcSpan, Type)]
        decompedOverloadedLits = catMaybes $ map decompExprLit exprLits ++ (map (\pat -> Just (getLoc pat, decompPatLit $ unLoc pat)) patLits)
          where exprLits :: [LHsExpr GhcTc]
                exprLits = concatMap universeBi . bagToList $ bnds

                decompExprLit :: LHsExpr GhcTc -> Maybe (SrcSpan, Type)
                decompExprLit (L loc (HsOverLit _ lit)) = Just (loc, ol_type (ol_ext lit))
                decompExprLit x = Nothing

                patLits :: [LPat GhcTc]
                patLits = concatMap universeBi . bagToList $ bnds

                decompPatLit (NPat llit _ _ _) = llit


        mkUnknownType :: IO Type
        mkUnknownType = do
          tUnique <- mkSplitUniqSupply 'x'
          return $ mkTyVarTy $ mkVanillaGlobal (mkSystemName (uniqFromSupply tUnique) (mkDataOcc "TypeNotFound")) (mkTyConTy anyTyCon)

        getFixities :: Ghc [(Module, (OccName, GHC.Fixity))]
        getFixities = do env <- getSession
                         pit <- liftIO $ eps_PIT <$> hscEPS env
                         let hpt = hsc_HPT env
                             ifaces = moduleEnvElts pit ++ map hm_iface (eltsUDFM hpt)
                         return $ concatMap (\mi -> map (mi_module mi, ) $ mi_fixities mi) ifaces

extractExprIds :: LHsBinds GhcTc -> [Located Id]
        -- expressions like HsRecFld are removed from the typechecked representation, they are replaced by HsVar
extractExprIds = catMaybes . map (\case L l (HsVar _ (L _ n) :: HsExpr GhcTc) -> Just (L l n)
                                        L l (HsWrap _ _ (HsVar _ (L _ n))) -> Just (L l n)
                                        _ -> Nothing
                                 ) . concatMap universeBi . bagToList

extractSigIds :: LHsBinds GhcTc -> [(SrcSpan,Id)]
extractSigIds = filter (isGoodSrcSpan . fst)
                  . concat
                  . map (\case L l bs@(AbsBinds { abs_sig = True } :: HsBind GhcTc)
                                 -> map (l,) $ concatMap getImplVars (abs_ev_binds bs)
                               _ -> [] )
                  . concatMap universeBi . bagToList
  where getImplVars (EvBinds evbnds) = catMaybes $ map getEvVar $ bagToList evbnds
        getImplVars _                = []
        getEvVar (EvBind lhs _ False) = Just lhs
        getEvVar _                    = Nothing

extractSigBindIds :: LHsBinds GhcTc -> [(SrcSpan,Id)]
extractSigBindIds = filter (isGoodSrcSpan . fst)
                      . catMaybes
                      . map (\case L l (IPBind _ (Right id) _ :: IPBind GhcTc) -> Just (l,id)
                                   _                                         -> Nothing )
                      . concatMap universeBi
                      . bagToList
