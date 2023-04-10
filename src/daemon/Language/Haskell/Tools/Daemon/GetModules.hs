{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications, LiberalTypeSynonyms #-}

-- | Collecting modules contained in a module collection (library, executable, testsuite or
-- benchmark). Gets names, source file locations, compilation and load flags for these modules.
module Language.Haskell.Tools.Daemon.GetModules where

import Control.Exception (Exception, throw)
import Control.Reference ((^.))
import Data.Char (isUpper)
import Data.Function (on)
import Data.List
import qualified Data.Map as Map (fromList)
import Data.Maybe (Maybe(..), maybe, catMaybes)
import Distribution.Compiler (AbiTag(..), unknownCompilerInfo, buildCompilerId, PerCompilerFlavor(..))
import Distribution.ModuleName (fromString, ModuleName, components)
import Distribution.Package (Dependency(..), PackageName(..), pkgName, unPackageName)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.System (buildPlatform)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec(..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Verbosity (silent)
import Language.Haskell.Extension as Cabal (Language(..), KnownExtension(..), Extension(..))
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath

import DynFlags
import qualified DynFlags as GHC
import GHC hiding (ModuleName)
import CmdLineParser

import Language.Haskell.Tools.Daemon.MapExtensions (translateExtension, setExtensionFlag', unSetExtensionFlag')
import Language.Haskell.Tools.Daemon.Representation
import Debug.Trace (trace, traceShowId)

-- | Gets all ModuleCollections from a list of source directories. It also orders the source directories that are package roots so that
-- they can be loaded in the order they are defined (no backward imports). This matters in those cases because for them there can be
-- special compilation flags.
getAllModules :: [FilePath] -> IO [ModuleCollection ModuleNameStr]
getAllModules pathes = trace "Piyush :: Inside getAllModules" $ orderMCs . concat <$> mapM getModules (map normalise pathes)

-- | Sorts model collection in an order to remove all backward references.
-- Works because module collections defined by directories cannot be recursive.
orderMCs :: [ModuleCollection k] -> [ModuleCollection k]
orderMCs = sortBy compareMCs
  where compareMCs :: ModuleCollection k -> ModuleCollection k -> Ordering
        compareMCs mc _ | DirectoryMC _ <- (_mcId mc) = GT
        compareMCs _ mc | DirectoryMC _ <- (_mcId mc) = LT
        compareMCs mc1 mc2 | (_mcId mc2) `elem` (_mcDependencies mc1) = GT
        compareMCs mc1 mc2 | (_mcId mc1) `elem` (_mcDependencies mc2) = LT
        compareMCs _ _ = EQ


-- | Get modules of the project with the indicated root directory.
-- If there is a cabal file, it uses that, otherwise it just scans the directory recursively for haskell sourcefiles.
-- Only returns the non-boot haskell modules, the boot modules will be found during loading.
getModules :: FilePath -> IO [ModuleCollection ModuleNameStr]
getModules root
  = do 
       files <- trace "Piyush :: Inside getModules" $ listDirectory root
       case find (\p -> takeExtension p == ".cabal") files of
          Just cabalFile -> trace "Piyush :: Inside getAllModules :: Use cabal file" $ modulesFromCabalFile root cabalFile
          Nothing        -> do mods <- modulesFromDirectory root root
                               return [ModuleCollection (DirectoryMC root) False root [root] [] (modKeys mods) return return []]
  where modKeys mods = Map.fromList $ map (, ModuleNotLoaded NoCodeGen True) mods

-- | Load the module giving a directory. All modules loaded from the folder and subfolders.
modulesFromDirectory :: FilePath -> FilePath -> IO [String]
-- now recognizing only .hs files
modulesFromDirectory root searchRoot = concat <$> (mapM goOn =<< listDirectory searchRoot)
  where goOn fp = let path = searchRoot </> fp
                   in do isDir <- doesDirectoryExist path
                         if isDir
                           then modulesFromDirectory root path
                           else if takeExtension path == ".hs"
                                  then return [concat $ intersperse "." $ splitDirectories $ dropExtension $ makeRelative root path]
                                  else return []

srcDirFromRoot :: FilePath -> String -> FilePath
srcDirFromRoot fileName "" = fileName
srcDirFromRoot fileName moduleName
  = srcDirFromRoot (takeDirectory fileName) (dropWhile (/= '.') $ dropWhile (== '.') moduleName)

-- | Load the module using a cabal file. The modules described in the cabal file will be loaded.
-- The flags and extensions set in the cabal file will be used by default.
modulesFromCabalFile :: FilePath -> FilePath -> IO [ModuleCollection ModuleNameStr]
-- now adding all conditional entries, regardless of flags
modulesFromCabalFile root cabal = (getModules . setupFlags <$> readGenericPackageDescription silent (root </> cabal))
  where getModules pkg = maybe [] (maybe [] (:[]) . toModuleCollection pkg) (library pkg)
                           ++ catMaybes (map (toModuleCollection pkg) (executables pkg))
                           ++ catMaybes (map (toModuleCollection pkg) (testSuites pkg))
                           ++ catMaybes (map (toModuleCollection pkg) (benchmarks pkg))

        toModuleCollection :: ToModuleCollection tmc => PackageDescription -> tmc -> Maybe (ModuleCollection ModuleNameStr)
        toModuleCollection PackageDescription{ buildTypeRaw = Just Custom } _
          = throw $ UnsupportedPackage "'build-type: custom' setting in cabal file"
        toModuleCollection pkg tmc
          = let bi = getBuildInfo tmc
                packageName = pkgName $ package pkg
             in if buildable bi
                  then Just $ ModuleCollection (mkModuleCollKey packageName tmc) False
                                root
                                (map (normalise . (root </>)) $ hsSourceDirs bi)
                                (map (\(mn, fs) -> (moduleName mn, fs)) $ getModuleSourceFiles tmc)
                                (Map.fromList $ map modRecord $ getModuleNames tmc)
                                (trace "Piyush :: Inside toModuleCollection :: FlagsFromBuildInfo" $ flagsFromBuildInfo bi)
                                (trace "Piyush :: Inside toModuleCollection :: LoadFlagsFromBuildInfo" $ loadFlagsFromBuildInfo bi)
                                (map (\(Dependency pkgName _ _) -> LibraryMC (unPackageName pkgName)) (targetBuildDepends bi))
                  else Nothing
          where modRecord mn = ( moduleName mn, ModuleNotLoaded NoCodeGen (needsToCompile tmc mn) )
        moduleName = concat . intersperse "." . components
        setupFlags = either (\deps -> error $ "Missing dependencies: " ++ show deps) fst
                       . finalizePD (mkFlagAssignment [])
                                    (ComponentRequestedSpec True True) (const True) buildPlatform
                                    (unknownCompilerInfo buildCompilerId NoAbiTag) []

data UnsupportedPackage = UnsupportedPackage String
  deriving Show

instance Exception UnsupportedPackage

-- | Extract module-related information from different kind of package components (library,
-- executable, test-suite or benchmark).
class ToModuleCollection t where
  -- | Creates a key for registering this package component.
  mkModuleCollKey :: PackageName -> t -> ModuleCollectionId
  -- | Gets the build info field from a package component.
  getBuildInfo :: t -> BuildInfo
  -- | Get the names of all the modules used by this package component.
  getModuleNames :: t -> [ModuleName]
  -- | Gets if some of the modules are defined is source files that are not in the expected
  -- location or named as expected.
  getModuleSourceFiles :: t -> [(ModuleName, FilePath)]
  getModuleSourceFiles _ = []
  -- | Checks if a module is exposed by the package component, so it is necessary to compile.
  -- Some of the components may have modules that are only conditionally imported by other modules.
  needsToCompile :: t -> ModuleName -> Bool
  -- | Gets the Main module in the case of executables, benchmarks and test suites.
  getMain :: t -> String
  getMain l = getMain' (getBuildInfo l)

instance ToModuleCollection Library where
  mkModuleCollKey pn _ = LibraryMC (unPackageName pn)
  getBuildInfo = libBuildInfo
  getModuleNames = explicitLibModules
  needsToCompile l m = m `elem` exposedModules l

instance ToModuleCollection Executable where
  mkModuleCollKey pn exe = ExecutableMC (unPackageName pn) (unUnqualComponentName $ exeName exe)
  getBuildInfo = buildInfo
  getModuleNames exe = fromString (getMain exe) : exeModules exe
  needsToCompile exe mn = components mn == [getMain exe]
  getModuleSourceFiles exe = [(fromString (getMain exe), modulePath exe)]

instance ToModuleCollection TestSuite where
  mkModuleCollKey pn test = TestSuiteMC (unPackageName pn) (unUnqualComponentName $ testName test)
  getBuildInfo = testBuildInfo
  getModuleNames exe = fromString (getMain exe) : testModules exe
  needsToCompile exe mn = components mn == [getMain exe]
  getModuleSourceFiles exe
    = case testInterface exe of
        TestSuiteExeV10 _ fp -> [(fromString (getMain exe), fp)]
        _ -> []
  getMain t = case testInterface t of
        TestSuiteLibV09 _ mod -> intercalate "." $ components mod
        _ -> getMain' (getBuildInfo t)

instance ToModuleCollection Benchmark where
  mkModuleCollKey pn test = BenchmarkMC (unPackageName pn) (unUnqualComponentName $ benchmarkName test)
  getBuildInfo = benchmarkBuildInfo
  getModuleNames exe = fromString (getMain exe) : benchmarkModules exe
  needsToCompile exe mn = components mn == [getMain exe]
  getModuleSourceFiles exe
    = case benchmarkInterface exe of
        BenchmarkExeV10 _ fp -> [(fromString (getMain exe), fp)]
        _ -> []

-- | A default method of getting the main module using the ghc-options field, checking for the
-- option -main-is.
getMain' :: BuildInfo -> String
getMain' bi
  = case ls of _:e:_ -> intercalate "." $ filter (isUpper . head) $ groupBy ((==) `on` (== '.')) e
               _ -> "Main"
  where ls = dropWhile (/= "-main-is") (getOptions $ options bi)
        getOptions (PerCompilerFlavor _ a) = a

-- | Checks if the module collection created from a folder without .cabal file.
isDirectoryMC :: ModuleCollectionId -> Bool
isDirectoryMC DirectoryMC{} = True
isDirectoryMC _ = False

-- | Modify the dynamic flags to match the dependencies requested in the .cabal file.
applyDependencies :: [ModuleCollectionId] -> [ModuleCollectionId] -> DynFlags -> DynFlags
applyDependencies mcs deps dfs
  = dfs { GHC.packageFlags = GHC.packageFlags dfs ++ (catMaybes $ map (dependencyToPkgFlag mcs) deps) }

-- | Only use the dependencies that are explicitely enabled. (As cabal does opposed to as ghc does.)
onlyUseEnabled :: DynFlags -> DynFlags
onlyUseEnabled = GHC.setGeneralFlag' GHC.Opt_HideAllPackages

-- | Transform dependencies of a module collection into the package flags of the GHC API
dependencyToPkgFlag :: [ModuleCollectionId] -> ModuleCollectionId -> Maybe (GHC.PackageFlag)
dependencyToPkgFlag mcs lib@(LibraryMC pkgName)
  = if lib `notElem` mcs
      then Just $ GHC.ExposePackage pkgName (GHC.PackageArg pkgName) (GHC.ModRenaming True [])
      else Nothing
dependencyToPkgFlag _ _ = Nothing

-- | Sets the configuration for loading all the modules from the whole project. Combines the
-- configuration of all package fragments. This solution is not perfect (it would be better to load
-- all package fragments separately), but it is how it works. See 'loadFlagsFromBuildInfo'.
setupLoadFlags :: [ModuleCollectionId] -> [FilePath]
                    -> [ModuleCollectionId] -> (DynFlags -> IO DynFlags) -> DynFlags -> IO DynFlags
-- need to be strict here, otherwise the previous modules cannot be garbage collected
setupLoadFlags !ids !roots !allDeps !flags dfs = applyDependencies ids allDeps . selectEnabled <$> flags dfs
  where selectEnabled = if any (\((mcId,mcRoot),rest) -> isDirectoryMC mcId && isIndependentMc mcRoot rest) (breaks (zip ids roots))
                          then id
                          else onlyUseEnabled
          where breaks :: [a] -> [(a,[a])]
                breaks [] = []
                breaks (e:rest) = (e,rest) : map (\(x,ls) -> (x,e:ls)) (breaks rest)
        isIndependentMc root rest = not $ any (`isPrefixOf` root) (map snd rest)

-- | Collects the compilation options and enabled extensions from Cabal's build info representation.
-- This setup will be used when loading all packages in the project. See 'setupLoadFlags'.
loadFlagsFromBuildInfo :: BuildInfo -> DynFlags -> IO DynFlags
loadFlagsFromBuildInfo bi@BuildInfo{ cppOptions } df
  = do (df',unused,warnings) <- parseDynamicFlags df (map (L noSrcSpan) $ cppOptions)
       mapM_ putStrLn (map (unLoc . warnMsg) warnings ++ map (("Flag is not used: " ++) . unLoc) unused)
       return (setupLoadExtensions df')
  where setupLoadExtensions = foldl (.) id (map setExtensionFlag' $ catMaybes $ map translateExtension loadExtensions)
        loadExtensions = [PatternSynonyms | patternSynonymsNeeded] ++ [ExplicitNamespaces | explicitNamespacesNeeded]
                           ++ [PackageImports | packageImportsNeeded] ++ [CPP | cppNeeded] 
                           ++ [MagicHash | magicHashNeeded]
        explicitNamespacesNeeded = not $ null $ map EnableExtension [ExplicitNamespaces, TypeFamilies, TypeOperators] `intersect` usedExtensions bi
        patternSynonymsNeeded = EnableExtension PatternSynonyms `elem` usedExtensions bi
        packageImportsNeeded = EnableExtension PackageImports `elem` usedExtensions bi
        cppNeeded = EnableExtension CPP `elem` usedExtensions bi
        magicHashNeeded = EnableExtension MagicHash `elem` usedExtensions bi

-- | Collects the compilation options and enabled extensions from Cabal's build info representation
-- for a single module. See 'compileInContext'.
flagsFromBuildInfo :: BuildInfo -> DynFlags -> IO DynFlags
-- the import pathes are already set globally
flagsFromBuildInfo bi@BuildInfo{ options } df
  = do 
    (df',unused,warnings) <- parseDynamicFlags df (map (L noSrcSpan) $ getOptions options)
    mapM_ putStrLn (map (unLoc . warnMsg) warnings ++ map (("Flag is not used: " ++) . unLoc) unused)

    let res = (flip lang_set (toGhcLang =<< defaultLanguage bi))
                  $ foldl (.) id (map (\case EnableExtension ext -> setEnabled True ext
                                             DisableExtension ext -> setEnabled False ext
                                  ) (usedExtensions bi))
                  $ foldr (.) id (map (setEnabled True) (languageDefault (defaultLanguage bi)))
                  $ df'
    res1 <- return $ res { extensions = trace ("PiyushRes :: " ++ (show $ extensions res)) $ extensions res }
    return $ res1 
  where toGhcLang Cabal.Haskell98 = Just GHC.Haskell98
        toGhcLang Cabal.Haskell2010 = Just GHC.Haskell2010
        toGhcLang _ = Nothing

        getOptions (PerCompilerFlavor _ a) = a


        -- We don't put the default settings (ImplicitPrelude, MonomorphismRestriction) here
        -- because that overrides the opposite extensions (NoImplicitPrelude, NoMonomorphismRestriction)
        -- enabled in modules.
        languageDefault (Just Cabal.Haskell2010)
          = [ DatatypeContexts, DoAndIfThenElse, EmptyDataDecls, ForeignFunctionInterface
            , PatternGuards, RelaxedPolyRec, TraditionalRecordSyntax ]
        -- Haskell 98 is the default
        languageDefault _
          = [ DatatypeContexts, NondecreasingIndentation, NPlusKPatterns, TraditionalRecordSyntax ]

        setEnabled enable ext
          = case translateExtension ext of
              Just e -> (if enable then setExtensionFlag' else unSetExtensionFlag') e
              Nothing -> id
