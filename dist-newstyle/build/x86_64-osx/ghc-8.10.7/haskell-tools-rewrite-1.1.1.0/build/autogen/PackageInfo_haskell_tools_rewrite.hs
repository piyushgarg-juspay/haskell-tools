{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_haskell_tools_rewrite (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "haskell_tools_rewrite"
version :: Version
version = Version [1,1,1,0] []

synopsis :: String
synopsis = "Facilities for generating new parts of the Haskell-Tools AST"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/haskell-tools/haskell-tools"
