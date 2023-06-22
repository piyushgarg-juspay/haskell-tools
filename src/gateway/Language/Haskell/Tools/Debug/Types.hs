{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Debug.Types where 

import Control.Monad.State
import GHC hiding (loadModule, ModuleName, DataDecl)
import Language.Haskell.Tools.Debug.DebugGhcAST ()
import Language.Haskell.Tools.Debug.RangeDebugInstances ()
import Language.Haskell.Tools.Refactor as HT hiding (ModuleName)
import Bag (bagToList)
import Data.Generics.Uniplate.Operations (universeBi)
import qualified Data.Map as Map
import Language.Haskell.Tools.BackendGHC.GHCUtils (getTopLevelId)
import UniqSupply as GHC (uniqFromSupply, mkSplitUniqSupply)
import Name as GHC hiding (varName)
import TysWiredIn as GHC (anyTyCon)
import TcEvidence as GHC (EvBind(..), TcEvBinds(..))
import Type as GHC (mkTyVarTy, mkTyConTy)
import Id as GHC (mkVanillaGlobal)
import Control.Applicative (Alternative(..))

mkUnknownType :: IO GHC.Type
mkUnknownType = do
  tUnique <- mkSplitUniqSupply 'x'
  return $ mkTyVarTy $ mkVanillaGlobal (mkSystemName (uniqFromSupply tUnique) (mkDataOcc "TypeNotFound")) (mkTyConTy anyTyCon)

extractTypes :: LHsBinds GhcTc -> [Id]
extractTypes = concatMap universeBi . bagToList

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

getType' :: GHC.Type -> GHC.Name -> TypecheckedSource -> Ghc Id
getType' ut name bnds = fromMaybe (mkVanillaGlobal name ut) <$> ((<|> Map.lookup name (ids bnds)) <$> getTopLevelId name)

ids :: TypecheckedSource -> Map.Map GHC.Name Id
ids bnds = Map.fromList $ map (\id -> (getName id, id)) $ extractTypes bnds
-- ids bnds = Map.fromList $ map (\id -> (show' $ getName id, show' $ idType id)) $ extractTypes bnds

getType :: GHC.Name -> TypecheckedSource -> Ghc Id
getType name bnds = do 
  ut <- liftIO mkUnknownType
  getType' ut name bnds

-- Code Snippet to fet the type
{- 
    
    -- liftIO $ putStrLn $ show rnSrc
    dflags <- getSessionDynFlags
    liftIO $ putStrLn $ "General Flags :: " ++ (show $ toList $ generalFlags dflags) 
    liftIO $ putStrLn "=============================================="
    let ids' :: [Id] = (concatMap universeBi . bagToList) tcSrc'
    liftIO $ print $ show' ids'
    liftIO $ putStrLn "=============================================="
    liftIO $ putStrLn $ show $ Map.toList $ ids tcSrc'
    liftIO $ putStrLn "=============================================="
    liftIO $ putStrLn $ show' $ (extractSigIds tcSrc' ++ extractSigBindIds tcSrc')
-}