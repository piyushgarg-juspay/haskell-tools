{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskell_tools_backend_ghc (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,1,1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/piyush.garg/.cabal/bin"
libdir     = "/Users/piyush.garg/.cabal/lib/x86_64-osx-ghc-8.10.7/haskell-tools-backend-ghc-1.1.1.0-inplace"
dynlibdir  = "/Users/piyush.garg/.cabal/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/piyush.garg/.cabal/share/x86_64-osx-ghc-8.10.7/haskell-tools-backend-ghc-1.1.1.0"
libexecdir = "/Users/piyush.garg/.cabal/libexec/x86_64-osx-ghc-8.10.7/haskell-tools-backend-ghc-1.1.1.0"
sysconfdir = "/Users/piyush.garg/.cabal/etc"

getBinDir     = catchIO (getEnv "haskell_tools_backend_ghc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskell_tools_backend_ghc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskell_tools_backend_ghc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskell_tools_backend_ghc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_tools_backend_ghc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_tools_backend_ghc_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
