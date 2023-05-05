{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.FlowCtxt where

import Language.Haskell.Tools.Refactor as Refactor hiding (LambdaCase)
import Language.Haskell.Tools.Refactor.Utils.Extensions
import Language.Haskell.Tools.Rewrite.Match.Binds

import GHC (Ghc(..))
import Module as GHC
import InstEnv as GHC
import Unify as GHC
import Type as GHC
import Name as GHC
import Var as GHC
import UniqSupply as GHC
import Unique as GHC
import TysWiredIn as GHC
import TysPrim as GHC
import PrelNames as GHC
import ConLike as GHC
import PatSyn as GHC
import BasicTypes as GHC

import Control.Reference
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List
import qualified Data.Map.Strict as SMap (empty, toList)

import Debug.Trace (trace, traceShowId)

import Outputable
import Language.Haskell.Tools.PrettyPrint

import Language.Haskell.Tools.BackendGHC.Exprs (trfExpr)
import Language.Haskell.Tools.BackendGHC.GHCUtils (occName, fromSrcText)
import Language.Haskell.Tools.BackendGHC.Names
import Language.Haskell.Tools.BackendGHC.Patterns (trfPattern)
import Language.Haskell.Tools.BackendGHC.Types (trfType)

import Language.Haskell.Tools.AST (Ann, AnnMaybeG, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

import Control.Monad.IO.Class

import Language.Haskell.Tools.Debug.RangeDebug
import Language.Haskell.Tools.Debug.RangeDebugInstances

-- import Language.Haskell.Tools.ASTDebug

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

changeContextTypeRefactoring :: RefactoringChoice
changeContextTypeRefactoring = ModuleRefactoring "ctxt" (localRefactoring changeContextType)

changeContextType :: LocalRefactoring
changeContextType moduleAST =
        do
            _ <- liftIO $ putStrLn $ srcInfoDebug $ (moduleAST)
            !newAST <- liftGhc $ (!~) (biplateRef) (updateContextType) (moduleAST)
            return newAST
            

updateContextType :: Assertion -> Ghc Assertion 
updateContextType assertion@(ClassAssert str typs) = 
    do 
        let name = showName str 
        if name == "Transactionable" 
            then 
                let newName = "MonadFlow"
                    (AnnList newTyps) = typs -- AnnListG ==> []
                in return $ mkClassAssert (mkName newName) newTyps
            else return assertion
updateContextType assertion = return assertion

