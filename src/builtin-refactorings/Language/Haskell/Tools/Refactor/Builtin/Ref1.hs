{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.Ref1 where

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

import Language.Haskell.Tools.Refactor.Builtin.FlowCtxt (updateContextType)

-- import Language.Haskell.Tools.ASTDebug

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

ref1Refactoring :: RefactoringChoice
ref1Refactoring = ModuleRefactoring "ref1" (localRefactoring ref1)

ref1 :: LocalRefactoring
ref1 moduleAST =
        do
            _ <- liftIO $ putStrLn $ srcInfoDebug $ (moduleAST)
            -- !newAST <- liftGhc $ (!~) (biplateRef) (ref1Change) (moduleAST)
            !newAST1 <- liftGhc $ (!~) (biplateRef) (updateContextType) (moduleAST)
            !newAST <- liftGhc $ (!~) (biplateRef) (ref1Change') (newAST1)
            return newAST
            

ls = ["val", "toKey", ".", "$"]

generateChain :: [String] -> [String] -> String 
generateChain []     _  = ""
generateChain (x:xs) ls = let
    t = generateChain xs ls
    in 
    if t == ""
    then 
        if x `elem` ls then t else x 
    else 
        if x `elem` ls then t else x ++ " $ " ++ t

getAllNames :: Expr -> [String]
getAllNames expr =  let 
                        !(nameParts :: [NamePart]) = expr ^? (biplateRef)
                    in fmap (\x -> x ^. simpleNameStr) nameParts

ref1Change' :: Rhs -> Ghc Rhs
ref1Change' rhs@(UnguardedRhs expr) = fmap mkUnguardedRhs $ checkAndUpdateFindOne expr
ref1Change' rhs                     = return rhs

checkAndUpdateFindOne :: Expr -> Ghc Expr 
checkAndUpdateFindOne expr@(InfixApp lhs' op' rhs') = 
    do 
        let op = showOp op' 
        if op == "$"
        then 
            let lhsNames = getAllNames lhs'
            in
                if length lhsNames <= 2 && "findOne" `elem` lhsNames -- ESQ.findOne
                then 
                    let newCall = "KVC.findWithKVConnector dbConf updatedMeshCfg " ++ checkAndUpdateWhere rhs'
                    in  return $ mkVar $ mkName $ newCall 
                else return expr
        else return expr
checkAndUpdateFindOne expr = return expr

checkAndUpdateWhere :: Expr -> String 
checkAndUpdateWhere expr = 
    let allExprs :: [Expr] = expr ^? (biplateRef)
        changedEqs = fmap ref1Change'' allExprs
        reqEqs = filter (\x -> length x > 0) changedEqs
        finalStr = foldr (\x r -> x ++ r) "" reqEqs
    in 
        "[" ++ finalStr ++ "]"


ref1Change'' :: Expr -> String
ref1Change'' app@(InfixApp lhs' op' rhs') =
    do 
        let op = showOp op' 
        if op == "==."
            then  
                let 
                    !(lhsNameParts :: [NamePart]) = lhs' ^? (biplateRef)
                    !lhsNames = fmap (\x -> x ^. simpleNameStr) lhsNameParts
                    !(rhsNameParts :: [NamePart]) = rhs' ^? (biplateRef)
                    !rhsNames = fmap (\x -> x ^. simpleNameStr) rhsNameParts
                    !newFunCall = "Is (\\x -> x." ++ lhsNames!!2 ++ ") (Eq $ " ++ generateChain rhsNames ls ++ ")"
                    !newExpr = mkVar $ mkName $ newFunCall 
                    -- !_ = trace ("LHS = " ++ show lhs') () 
                    -- !_ = trace ("RHS = " ++ show rhs') () 
                    -- !_ = trace ("LHS Function Names = " ++ show lhsNames) ()
                    -- !_ = trace ("RHS Function Names = " ++ show rhsNames) () 
                    -- !_ = trace ("New Function Call = " ++ newFunCall) () 
                in 
                newFunCall
            else ""
ref1Change'' app = ""



ref1Change :: Expr -> Ghc Expr 
ref1Change app@(InfixApp lhs' op' rhs') =
    do 
        let op = showOp op' 
        if op == "==."
            then  
                let 
                    !(lhsNameParts :: [NamePart]) = lhs' ^? (biplateRef)
                    !lhsNames = fmap (\x -> x ^. simpleNameStr) lhsNameParts
                    !(rhsNameParts :: [NamePart]) = rhs' ^? (biplateRef)
                    !rhsNames = fmap (\x -> x ^. simpleNameStr) rhsNameParts
                    !newFunCall = "Is (\\x -> x." ++ lhsNames!!2 ++ ") (Eq $ " ++ generateChain rhsNames ls ++ ")"
                    !newExpr = mkVar $ mkName $ newFunCall 
                    -- !_ = trace ("LHS = " ++ show lhs') () 
                    -- !_ = trace ("RHS = " ++ show rhs') () 
                    -- !_ = trace ("LHS Function Names = " ++ show lhsNames) ()
                    -- !_ = trace ("RHS Function Names = " ++ show rhsNames) () 
                    -- !_ = trace ("New Function Call = " ++ newFunCall) () 
                in 
                return $ mkList $ [newExpr]
            else return app 
ref1Change app = return app

