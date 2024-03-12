{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_haskell_tools_backend_ghc (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "haskell_tools_backend_ghc"
version :: Version
version = Version [1,1,1,0] []

synopsis :: String
synopsis = "Creating the Haskell-Tools AST from GHC's representations"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/nboldi/haskell-tools"
