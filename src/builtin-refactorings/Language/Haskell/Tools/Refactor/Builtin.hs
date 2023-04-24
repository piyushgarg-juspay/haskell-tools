{-# LANGUAGE MonoLocalBinds #-}

module Language.Haskell.Tools.Refactor.Builtin ( builtinRefactorings, builtinQueries ) where

import Language.Haskell.Tools.Refactor (RefactoringChoice, QueryChoice)
import Language.Haskell.Tools.Refactor.Builtin.ExtractBinding (extractBindingRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.FloatOut (floatOutRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.GenerateExports (generateExportsRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.GenerateTypeSignature (generateTypeSignatureRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.InlineBinding (inlineBindingRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions (organizeExtensionsRefactoring, projectOrganizeExtensionsRefactoring, highlightExtensionsQuery)
import Language.Haskell.Tools.Refactor.Builtin.OrganizeImports (organizeImportsRefactoring, projectOrganizeImportsRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.RenameDefinition (renameDefinitionRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.GetMatches (getMatchesQuery)
import Language.Haskell.Tools.Refactor.Builtin.AutoCorrect
import Language.Haskell.Tools.Refactor.Builtin.EPNG (changeEpngTypeRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.EPNG2 (changeEpngTypeRefactoring2)
import Language.Haskell.Tools.Refactor.Builtin.PrintTree (printTreeRefactoring)
import Language.Haskell.Tools.Refactor.Builtin.ReturnToLet (changePureToLetRefactoring)

builtinRefactorings :: [RefactoringChoice]
builtinRefactorings
  = [ organizeImportsRefactoring
    , projectOrganizeImportsRefactoring
    , inlineBindingRefactoring
    , generateTypeSignatureRefactoring
    , renameDefinitionRefactoring
    , generateExportsRefactoring
    , floatOutRefactoring
    , extractBindingRefactoring
    , organizeExtensionsRefactoring
    , projectOrganizeExtensionsRefactoring
    , autoCorrectRefactoring
    , changeEpngTypeRefactoring
    , printTreeRefactoring
    , changeEpngTypeRefactoring2
    , changePureToLetRefactoring
    ]

builtinQueries :: [QueryChoice]
builtinQueries = [ getMatchesQuery, highlightExtensionsQuery ]
