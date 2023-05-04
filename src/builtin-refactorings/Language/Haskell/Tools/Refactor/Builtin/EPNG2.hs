{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.EPNG2 where

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

changeEpngTypeRefactoring2 :: RefactoringChoice
changeEpngTypeRefactoring2 = ModuleRefactoring "epng2" (localRefactoring changeEpngType2)

changeEpngType2 :: LocalRefactoring
changeEpngType2 moduleAST =
        do
            _ <- liftIO $ putStrLn $ srcInfoDebug $ (moduleAST)
            !newAST <- liftGhc $ (!~) (biplateRef) (updateDotType) (moduleAST)
            return newAST
            

mp :: [(String, String, String)]
mp = [
          ("TxnDetail", "_id", "Just")
        -- , ("TxnDetail", "name", "Just")
        -- , ("TxnDetail2", "_id", "Just")
     ]

getFnNamefromMap :: String -> String -> Maybe String
getFnNamefromMap tyName lnName = go mp
    where go []     = Nothing 
          go ((y1, y2, y3):ys) = if tyName == y1 && lnName == y2 then Just $ y3 else go ys 

containsLensWithDefault :: Expr -> Ghc Bool 
containsLensWithDefault expr = 
    do 
        let !(exprs :: [String]) = trace "Exprs Obtained :: " $ traceShowId $ (expr ^? ((exprInner & exprOperator) &+& (exprOperator)) & operatorName & unqualifiedName & simpleNameStr)
            -- mexprs = filter go exprs
        if length exprs > 0 then return True else return False
          

updateDotType :: Expr -> Ghc Expr 
updateDotType expr@(App fun args) = 
    do 
        ty <- typeExpr args
        let (isGetField, fieldName) = checkIfGetField fun
        !_ <- liftIO $ putStrLn $ "UpdateDot:: Type = " ++ (showSDocUnsafe $ ppr $ ty) ++ " isGetField = " ++ (show isGetField) ++ " fieldName = " ++ (show fieldName)
        -- let dollarOp = mkUnqualOp "$"
        let dollarOp = mkQualOp ["Prelude"] "$"
            dType = showOutputable ty
            newFnName = getFnNamefromMap dType fieldName
        if isGetField && isJust newFnName
            then 
                -- let fnNameFromMap = mkVar $ mkName $ fromJust newFnName
                let fnNameFromMap = mkVar $ mkNormalName $ mkQualifiedName ["Prelude"] $ fromJust newFnName
                    [oldArgName]  = args ^? (exprName & simpleName & unqualifiedName & simpleNameStr)
                    newArgName    = if oldArgName == "z" then "z2" else oldArgName
                    newArg        = mkVar $ mkName newArgName
                    newField      = if fieldName == "_id" then "_id1" else fieldName
                    (ExplicitTypeApp f' typ') = fun
                    newTypeApp    = mkExplicitTypeApp f' $ mkPromotedStringType newField
                    newExpr       = mkApp newTypeApp newArg
                    y = trace ("Add changing function " ++ (fromJust newFnName)) $ mkInfixApp fnNameFromMap dollarOp newExpr
                    -- fnn = mkVar $ mkNormalName $ mkQualifiedName ["Prelude"] "Just"
                    -- fnn2 = mkLit $ mkIntLit 2
                    -- y = trace ("Add changing function " ++ (fromJust newFnName)) $ mkApp fnn fnn2
                in trace ("Changed dots" ++ show y) (return y)
                -- trace "Add changing function" $ (return expr)
        else return $ expr
updateDotType expr = return $ expr

checkIfGetField :: Expr -> (Bool, String)
checkIfGetField ex@(ExplicitTypeApp fun typ) = trace ("Inside checkGetField" ++ show ex) $
    let funName' = fun ^? (exprName & simpleName & unqualifiedName & simpleNameStr)
        funName = case funName' of 
                    [x] -> x 
                    _   -> ""
        typName' =  typ ^? (tpPromoted & promotedStringValue)
        typName = case typName' of 
                    [x] -> x 
                    _   -> ""
    in 
        if funName == "getField" && typName /= ""
            then (True, typName)
            else (False, "")
checkIfGetField x                         = trace ("pattern is " ++ show x) $ (False, "")