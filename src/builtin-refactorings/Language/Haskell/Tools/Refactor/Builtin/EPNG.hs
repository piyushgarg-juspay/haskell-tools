{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}


module Language.Haskell.Tools.Refactor.Builtin.EPNG where

import Language.Haskell.Tools.Refactor hiding (LambdaCase)
import Language.Haskell.Tools.Refactor.Utils.Extensions

import GHC (Ghc(..))

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

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

changeEpngTypeRefactoring :: RefactoringChoice
changeEpngTypeRefactoring = ModuleRefactoring "changeEpngType" (localRefactoring changeEpngType)

changeEpngType :: LocalRefactoring
changeEpngType moduleAST =
     do
        let opts = moduleAST ^? modDecl & annList
            ghcOpts' = filter (fun) (trace ("Total Decls in AST = " ++ (show (length opts))) $ opts)
            dummy = fmap (getValueBind) ghcOpts'
            -- dummy = opts & declValBind
            ghcOpts'' = filter (fun1) (trace ("Total ValueBinds in AST = " ++ (show (length ghcOpts'))) $ (dummy))
            -- dummy2 = fmap (_) ghcOpts''
            -- ghcOpts''' = filter (_) (trace ("Total SimpleBinds in AST = " ++ (show (length ghcOpts''))) $ (dummy2))
        let x = trace ("Latest valBindRHS in list = " ++ (show (length ghcOpts''))) $ moduleAST
        return x


fun :: Decl -> Bool
fun dec = case dec of 
    a@(ValueBinding _) -> True
    _                  -> False

fun1 :: ValueBind -> Bool 
fun1 dec = case dec of 
    a@(AST.Ann _ (AST.USimpleBind _ _ _)) -> True 
    a@(AST.Ann _ (AST.UFunBind _)) -> False 
    _              -> False

-- fun2 :: 

getValueBind :: Decl -> ValueBind
getValueBind (ValueBinding x) = x 
getValueBind _                = Prelude.error "Should never reach here while converting Decl -> ValueBind"