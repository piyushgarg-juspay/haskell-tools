{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.EPNG where

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

changeEpngTypeRefactoring :: RefactoringChoice
changeEpngTypeRefactoring = ModuleRefactoring "changeEpngType" (localRefactoring changeEpngType)

changeEpngType :: LocalRefactoring
changeEpngType moduleAST =
        do
            _ <- liftIO $ putStrLn $ srcInfoDebug $ (moduleAST)
            -- let   newAST  = (.-) (modDecl & annList & declValBind & valBindRhs & rhsExpr & exprStmts & annList) (updateLensType) (moduleAST)
            --       newAST' = (.-) (modDecl & annList & declValBind & valBindLocals & annJust & localBinds & annList & localVal & valBindRhs & rhsExpr & exprStmts & annList) (updateLensType) (newAST)
            !newAST <- liftGhc $ (!~) (biplateRef) (updateLensType) (moduleAST)
                        --     (biplateRef & rhsExpr) 
                        -- &+& (biplateRef & exprRhs) 
                        -- &+& (biplateRef & exprInner)
                        -- &+& (biplateRef & guardExpr)
                        -- &+& (biplateRef & guardRhs)
                        -- &+& (biplateRef & guardCheck)
                        -- &+& (biplateRef & annotateExpr)
                        -- &+& (biplateRef & tupleElems & annList)
                        -- &+& (biplateRef & listElems & annList)
                        -- &+& (biplateRef & exprLhs)
                        -- &+& (biplateRef & exprFun)
                        -- &+& (biplateRef & exprArg)
                        -- &+& (biplateRef & exprCond)
                        -- &+& (biplateRef & exprThen)
                        -- &+& (biplateRef & exprElse)
                        -- &+& (biplateRef & exprCase)) 
                        -- (updateLensType) (moduleAST)
            -- newAST' <- liftGhc $ (!~) (modDecl & annList & declValBind & valBindLocals & annJust & localBinds & annList & localVal & valBindRhs & rhsExpr & exprStmts & annList) (updateLensType) (newAST)
            -- return $ newAST
            return newAST
            

mp :: [(String, String, String)]
mp = [
          ("TxnDetail", "id", "show")
        , ("TxnDetail", "name", "Just")
        , ("TxnDetail", "desc", "maybeIntToMaybeText")
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
          

updateLensType :: Expr -> Ghc Expr 
updateLensType expr@(InfixApp lhs opr rhs) = 
    do 
        ty <- typeExpr lhs
        let oprSimpleName = (opr ^? (operatorName & unqualifiedName & simpleNameStr))
            y = (head oprSimpleName)
            defOprs = ["^."]
            dollarOp = mkUnqualOp "$"
            lhsType = showOutputable ty
            [rhsName] = (rhs ^? (exprName & simpleName & unqualifiedName & simpleNameStr))
            newFnName = getFnNamefromMap lhsType rhsName
        containsLens <- containsLensWithDefault lhs
        if y `elem` defOprs && isJust newFnName
            then 
                let fnNameFromMap = mkVar $ mkName $ fromJust newFnName
                in trace "LENSTOCHANGE" $ return $ mkInfixApp fnNameFromMap dollarOp expr 
        else if y == "$" && containsLens
            then do 
                    let [lhs'] = lhs ^? ((exprInner & exprLhs) &+& (exprLhs))
                        [opr'] = lhs ^? ((exprInner & exprOperator) &+& (exprOperator))
                        [rhs'] = lhs ^? ((exprInner & exprRhs) &+& (exprRhs))
                        oprSimpleName' = (opr' ^? (operatorName & unqualifiedName & simpleNameStr))
                    ty <- typeExpr lhs'
                    let y' = (head oprSimpleName')
                        lhsType' = showOutputable ty
                        [rhsName'] = (rhs' ^? (exprName & simpleName & unqualifiedName & simpleNameStr))
                        newFnName' = getFnNamefromMap lhsType' rhsName'
                    if isJust newFnName'
                        then 
                            let fnNameFromMap' = mkVar $ mkName $ fromJust newFnName'
                                fromMaybe      = mkVar $ mkName "fromMaybe"
                                lensOp = mkUnqualOp "^."
                                refactoredLens = mkInfixApp fnNameFromMap' dollarOp (mkInfixApp lhs' lensOp rhs')
                            in
                                return $ mkInfixApp (mkApp fromMaybe rhs) dollarOp (refactoredLens)
                        else return expr 
        else return $ expr
    -- trace ("STMT :: " ++ (srcInfoDebug $ expr) ++ "\n :: DONE") $ (return expr)
-- updateLensType expr@(PrefixApp opr rhs) = updateLensType rhs
-- updateLensType expr@(App fun arg) = updateLensType arg 
-- updateLensType expr@(Lambda patList inner) = updateLensType inner 
-- updateLensType expr@(Let lclBnd inner) = updateLensType inner 
-- updateLensType expr@(If cnd th el) = undefined
-- updateLensType expr@(MultiIf cnd th el) = undefined


    -- do 
    --     (shouldUpdate, updatedRhsExpr) <- checkAndRemoveIfDefFun expr 
    --     if shouldUpdate
    --         then 
    --             let updatedRhs = mkUnguardedRhs $ updatedRhsExpr
    --                 lclbnd = mkLocalValBind $ mkSimpleBind pat updatedRhs Nothing 
    --             in return $ mkLetStmt [lclbnd]
    --     else return $ stmt

updateLensType expr = return $ expr

checkAndRemoveIfDefFun :: Expr -> Ghc (Bool, Expr)
checkAndRemoveIfDefFun expr@(App fun args) =
    do 
        ty1 <- typeExpr expr
        ty2 <- typeExpr fun
        ty3 <- typeExpr args
        -- _ <- trace ("Semantic info obtained " ++ (showSDocUnsafe $ ppr $ args ^. semantics)) $ return ty1
        _ <- trace ("Type obtained " ++ (showSDocUnsafe $ ppr $ ty1) ++ " fun " ++ (showSDocUnsafe $ ppr $ ty2) ++ " args " ++ (showSDocUnsafe $ ppr $ ty3)) $ (return ty1)
        let funSimpleName = (fun ^? (exprName & simpleName & unqualifiedName & simpleNameStr)) 
            y = (head funSimpleName)
            -- pq = myFun expr
            -- -- z = trace ("Type Signature : " ++ (show $ pq)) $ (myFun pq)
            defFun = ["return", "pure"]
        if y `elem` defFun
            then return $ (True, args)
        else return $ (False, expr)
    -- let funSimpleName = (fun ^? (exprName & simpleName & unqualifiedName & simpleNameStr)) 
    --     y = (head funSimpleName)
    --     -- pq = myFun expr
    --     -- -- z = trace ("Type Signature : " ++ (show $ pq)) $ (myFun pq)
    --     defFun = ["return", "pure"]
    -- in
    --     if y `elem` defFun
    --         then return $ (True, args)
    --     else return $ (False, expr)
    -- where
    --     myfun :: Expr -> GHC.Type  
    --     myfun x = do
    --                 t <- typeExpr x
    --                 t

checkAndRemoveIfDefFun expr                = return $ (False, expr)   