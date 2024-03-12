{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_haskell_tools_ast (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "haskell_tools_ast"
version :: Version
version = Version [1,1,1,0] []

synopsis :: String
synopsis = "Haskell AST for efficient tooling"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/nboldi/haskell-tools"
