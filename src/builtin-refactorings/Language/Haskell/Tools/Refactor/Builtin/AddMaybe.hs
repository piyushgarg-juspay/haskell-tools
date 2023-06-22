{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.AddMaybe where

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
-- import Language.Haskell.Tools.ASTDebug.Instances

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

addMaybeRefactoring :: RefactoringChoice
addMaybeRefactoring = GatewayRefactoring "gw" (localRefactoring . addMaybe)

addMaybe :: [String] -> LocalRefactoring
addMaybe args moduleAST =
        do
            -- _ <- liftIO $ putStrLn $ srcInfoDebug $ moduleAST
            _ <- liftIO $ putStrLn $ "Arguments to refactoring = " ++ show args
            let typ = args !! 0
                fld = args !! 1
            !newAST <- liftGhc $ (!~) (biplateRef) (updateDataDecl typ fld) (moduleAST)
            return newAST

updateDataDecl :: String -> String -> Decl -> Ghc Decl 
updateDataDecl typ fld decl@(DataDecl _ _ (NameDeclHead name) (AnnList [RecordConDecl _ _]) _) = do 
    if showName name == typ 
        then (!~) (biplateRef) (updateFieldType fld) (decl)
        else return decl 
updateDataDecl _ _ decl = return decl

updateFieldType :: String -> FieldDecl -> Ghc FieldDecl
updateFieldType fld fldDecl@(FieldDecl (AnnList names) typ) = do 
    let isField = any (\x -> showName x == fld) names
        (typName:_) = fmap showName $ typ ^? biplateRef
        needMaybe = typName /= "Maybe"
        newType = mkTypeApp (mkVarType $ mkName "Maybe") typ
    if isField && needMaybe
        then return $ mkFieldDecl names newType
        else return fldDecl 
updateFieldType _ fldDecl = return fldDecl
