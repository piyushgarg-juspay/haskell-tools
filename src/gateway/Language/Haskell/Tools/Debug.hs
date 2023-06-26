{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Debug where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO(..))
import Control.Reference ((^.), biplateRef, (^?))
import Data.List.Split (splitOn)
import Data.Maybe (Maybe(..), fromJust)
import GHC.Generics (Generic(..))
import System.FilePath (pathSeparator, (</>), (<.>))
import Data.List (isInfixOf)

import GHC hiding (loadModule, ModuleName, DataDecl)
import GHC.Paths ( libdir )
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import StringBuffer (hGetStringBuffer, stringToStringBuffer)
import Outputable
import HscTypes
import TcRnDriver
import TcRnTypes
import TcRnMonad
import Data.IORef
import DynFlags
import Avail

import Language.Haskell.Tools.AST (NodeInfo(..), UDecl(..), )
import Language.Haskell.Tools.BackendGHC
import Language.Haskell.Tools.Debug.Utils
import Language.Haskell.Tools.Debug.DebugGhcAST ()
import Language.Haskell.Tools.Debug.RangeDebug (srcInfoDebug)
import Language.Haskell.Tools.Debug.RangeDebugInstances ()
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Refactor as HT hiding (ModuleName)
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import qualified Language.Haskell.GHC.ExactPrint as EP
import Data.Either

import System.IO.Strict as StrictIO (hGetContents)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import Data.Algorithm.DiffContext (prettyContextDiff, getContextDiff)
import System.IO
import System.Directory
import Text.PrettyPrint as PP (text, render)
import DynamicLoading (initializePlugins)
import EnumSet (toList)
import Bag (bagToList)
import Data.Generics.Uniplate.Operations (universeBi)
import qualified Data.Map as Map
import Id as GHC (Id, mkVanillaGlobal)
import Control.Applicative (Applicative(..), (<$>), Alternative(..))
import Language.Haskell.Tools.BackendGHC.GHCUtils (getTopLevelId)
import UniqSupply as GHC (uniqFromSupply, mkSplitUniqSupply)
import Type as GHC (Type, mkTyVarTy, mkTyConTy)
import Name as GHC hiding (varName)
import TysWiredIn as GHC (anyTyCon)
import TcEvidence as GHC (EvBind(..), TcEvBinds(..))

import GhcMonad (modifySession, withTempSession)
import GHCi (purgeLookupSymbolCache)
import NameCache (NameCache(..))
import Module (delModuleEnvList)
import Linker (unload)
import Finder (flushFinderCaches)

import Data.Time.Clock (getCurrentTime)

import Control.Concurrent.Async
import UnliftIO.Async
import Control.Concurrent
import System.Directory (copyFile)

import qualified Data.Aeson as Aeson

import System.Environment (lookupEnv, getEnvironment)
import Text.Read (readMaybe)


type ModuleName = String
type GhcS = StateT TypesState Ghc
type ConNameMap = Map.Map String String
type DepMap = Map.Map String [String]
type DiffMap = Map.Map String Bool
type FieldsMap = [(String, String, Bool)]

data TypesState = TypeState {
      allRecFields :: [String]
    , test :: Bool 
  }

-- | Should be only used for testing
demoRefactor :: String -> String -> [String] -> String -> IO ()
demoRefactor command workingDir args gatewayName = do
  -- Run the refactoring
  runGhc (Just libdir) $ do
    -- Setup flags
    initGhcFlagsForGateway
    _ <- useDirs [workingDir]
    _ <- useFlags args

    -- Create moduleNames for loading and fileNames for target
    let modFileNames = ["Types", "Transforms", "Flow"]
        modNames = fmap (\x -> "Gateway." ++ gatewayName ++ "." ++ x) modFileNames
        moduleName = modNames !! 0
        modFile = workingDir ++ (modFileNames !! 0) ++ ".hs"

    let filePath = workingDir ++ "Env.hs"
    target <- guessTarget filePath Nothing
    setTargets [target]

    -- Debugging prints
    liftIO $ print $ ("Module Names :: " ++ show modNames)
    flags <- getSessionDynFlags
    liftIO $ print ("Import Paths :: " ++ (show $ importPaths flags))
    liftIO $ print ("Load file :: " ++ modFile ++ " , Module :: " ++ moduleName)

    -- Load module and mod summary
    liftIO $ putStrLn "=========== parsed source:"
    ms <- loadGatewayModule modFile moduleName
    p <- parseModule ms
    let annots = pm_annotations $ p
        ms = pm_mod_summary p
    -- liftIO $ putStrLn $ show' $ pm_parsed_source p

    -- typecheck using temp session Dyn Flags for plugins
    modFlags <- return $ ms_hspp_opts ms
    (rnSrc', tcSrc', p', ms') <- withTempSession (addPluginFlags modFlags) $ 
      do 
        -- dflags <- getSessionDynFlags
        -- setSessionDynFlags $ dflags {pluginModNames = pluginModNames flags, pluginModNameOpts = pluginModNameOpts flags}

        -- Initialize plugins
        dflags <- getSessionDynFlags
        hsc_env <- getSession
        dflags <- liftIO (initializePlugins hsc_env dflags)
        ms' <- pure $ modSumNormalizeFlags $ ms { ms_hspp_opts = dflags }

        -- Parse Module
        p' <- parseModule ms'

        -- Debugging Prints
        flags <- getSessionDynFlags
        liftIO $ print $ "Plugin ModNames = " ++ (show' $ pluginModNames flags)
        liftIO $ print $ "Plugin ModNamesOpts = " ++ (show' $ pluginModNameOpts flags)
        flags <- return $ ms_hspp_opts ms
        liftIO $ print $ "Plugin ModNames from Module Flags = " ++ (show' $ pluginModNames flags)
        liftIO $ print $ "Plugin ModNamesOpts from Module Flags = " ++ (show' $ pluginModNameOpts flags)
        -- liftIO $ print $ "----------------------------------------------------"
        -- liftIO $ print $ "Parsed Module :: "
        -- liftIO $ print $ show' $ pm_parsed_source p

        liftIO $ putStrLn "=========== tokens:"
        -- liftIO $ putStrLn $ show $ Map.toList $ (fst annots)
        liftIO $ putStrLn "=========== comments:"
        -- liftIO $ putStrLn $ show' (snd annots)
        liftIO $ putStrLn "=========== renamed source:"

        (rnSrc', tcSrc') <- ((\t -> (tm_renamed_source t, typecheckedSource t)) <$> typecheckModule p')
                            `gcatch` \(e :: SomeException) -> error ("Some exception during typecheck : "  ++ show e)
        liftIO $ putStrLn $ "++++++++++ Typecheck with Plugins Success"
        return (rnSrc', tcSrc', p', ms')


    -- liftIO $ putStrLn $ show rnSrc
    -- dflags <- getSessionDynFlags
    -- liftIO $ putStrLn $ "General Flags :: " ++ (show $ toList $ generalFlags dflags) 
    -- error "stop"
    (rnSrc, tcSrc) <- ((\t -> (tm_renamed_source t, typecheckedSource t)) <$> typecheckModule p)
                         `gcatch` \(_ :: SomeException) -> forcedTypecheck ms p
    -- liftIO $ putStrLn $ show rnSrc

    liftIO $ putStrLn $ "++++++++++ Typecheck w/o Plugins Success"

    -- liftIO $ putStrLn $ show (fromJust $ tm_renamed_source t)
    liftIO $ putStrLn "=========== typechecked source:"
    -- liftIO $ putStrLn $ show tcSrc

    let hasCPP = Cpp `xopt` ms_hspp_opts ms

    liftIO $ putStrLn "=========== parsed:"
    --transformed <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule (pm_parsed_source p)
    parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms (pm_parsed_source p)
    -- liftIO $ putStrLn $ srcInfoDebug parseTrf

    liftIO $ putStrLn "=========== typed:"
    -- let !a       = trfModuleRename ms parseTrf (fromJust $ rnSrc) (pm_parsed_source p)
    -- let !renamed = runTrf (fst annots) (getPragmaComments $ snd annots) a
    -- renamed' <- renamed
    -- liftIO $ putStrLn $ srcInfoDebug renamed'
    liftIO $ putStrLn "Renamed module done"
    transformed <- addTypeInfos tcSrc' =<< runTrf (fst annots) (getPragmaComments $ snd annots) (trfModuleRename ms parseTrf (fromJust $ rnSrc) (pm_parsed_source p))
    -- liftIO $ putStrLn $ srcInfoDebug transformed

    liftIO $ putStrLn "=========== ranges fixed:"
    sourceOrigin <- if hasCPP then liftIO $ hGetStringBuffer (workingDir </> map (\case '.' -> pathSeparator; c -> c) moduleName <.> "hs")
                              else return (fromJust $ ms_hspp_buf $ pm_mod_summary p)
    let commented = fixRanges $ placeComments (fst annots) (getNormalComments $ snd annots) $ fixMainRange sourceOrigin transformed
    -- liftIO $ putStrLn $ srcInfoDebug commented

    liftIO $ putStrLn "=========== cut up:"
    let cutUp = cutUpRanges commented
    -- liftIO $ putStrLn $ srcInfoDebug cutUp
    -- liftIO $ putStrLn $ show $ getLocIndices cutUp
    -- liftIO $ putStrLn $ show $ mapLocIndices sourceOrigin (getLocIndices cutUp)

    liftIO $ putStrLn "=========== sourced:"
    let sourced = (if hasCPP then extractStayingElems else id) $ rangeToSource sourceOrigin cutUp
    -- liftIO $ putStrLn $ srcInfoDebug sourced
    liftIO $ putStrLn "=========== pretty printed:"
    -- let prettyPrinted = prettyPrint sourced
    -- liftIO $ putStrLn prettyPrinted

    let (dataDecls :: [Decl]) = sourced ^? biplateRef
        (recTypes ::[(String, String)]) = foldr recTypesOnly [] dataDecls
        (recResTypes' :: [(String, String)]) = filter (\(typ, _) -> not (isInfixOf "req" typ || isInfixOf "Req" typ)) recTypes
        (recResTypes'' :: [(String, String)]) = recResTypes'

    -- liftIO $ putStrLn $ "Record Types in File with Fields that are mandatory right now :: " ++ show recTypes
    liftIO $ putStrLn $ "Response Types in File with Fields that are mandatory right now :: " ++ show recResTypes''

    -- Enable all Errors
    dflags <- getSessionDynFlags
    let dynflags = (flip gopt_unset Opt_DeferTypeErrors . flip gopt_unset Opt_DeferTypedHoles . flip gopt_unset Opt_DeferOutOfScopeVariables) dflags
    setSessionDynFlags dynflags
    -- hsc_env <- getSession
    hsc_env_flags <- getSessionDynFlags

    -- create temp directory
    liftIO $ createDirectoryIfMissing False $ workingDir ++ "temp/"

    cachedFieldsMap <- liftIO $ getCachedFieldsMap gatewayName
    liftIO $ putStrLn $ show cachedFieldsMap

    concurrentModules' <- liftIO $ lookupEnv "HT_GATEWAY_CON"
    let concurrentModules = maybe (Just 5) (readMaybe) concurrentModules'

    -- Generate fieldsMap concurrently
    (fieldsMap :: FieldsMap) <- liftIO $ concat <$> pooledForConcurrentlyN (fromJust concurrentModules) recResTypes'' (\(typ, fld) -> do
      -- Check if already cached result
      let (!isCached, !cacheRes) = isCachedField (typ, fld) cachedFieldsMap
      !_ <- liftIO $ putStrLn $ "Checked in cache"

      if isCached then do 
        !_ <- putStrLn $ "Found in Cache " ++ show (typ, fld, cacheRes)
        return [(typ, fld, cacheRes)]
      else do 
        !_ <- putStrLn $ "Not Found in Cache " ++ show (typ, fld)

        -- Create constants
        let !command  = "gw " ++ typ ++ " " ++ fld
            !modDir   = workingDir ++ "temp/temp_" ++ typ ++ "_" ++ fld ++ "/"
            !modFile1 = modDir ++ "Types.hs"
            !modFile2 = modDir ++ "Transforms.hs"
            !modFile3 = modDir ++ "Flow.hs"
            !mod1     = "Gateway." ++ gatewayName ++ ".Types" 
            !mod2     = "Gateway." ++ gatewayName ++ ".Transforms" 
            !mod3     = "Gateway." ++ gatewayName ++ ".Flow" 

        -- Created constants log
        liftIO $ putStrLn $ "Done with creating constants for " ++ show (typ, fld)

        -- create temp directory
        liftIO $ createDirectoryIfMissing False modDir

        -- Created Directory log
        liftIO $ putStrLn $ "Done with creating temp directory for " ++ show (typ, fld)

        -- copy files
        liftIO $ copyFile (workingDir ++ "Types.hs")      modFile1
        liftIO $ copyFile (workingDir ++ "Transforms.hs") modFile2
        liftIO $ copyFile (workingDir ++ "Flow.hs")       modFile3

        -- Copy finish log
        liftIO $ putStrLn $ "Done with copying the files for " ++ show (typ, fld)

        -- run a separate GHC session
        !result <- liftIO $ runGhc (Just libdir) $ do 
          !_ <- initGhcFlagsForGateway' True False
          !_ <- useDirs [modDir]
          !_ <- useFlags args
          !_ <- setTempFilesFlags True
          
          !oldFlags <- getSessionDynFlags
          !_ <- setSessionDynFlags $ setTmpDir modDir oldFlags

          -- Set session log
          liftIO $ putStrLn $ "Done with setting up session for " ++ show (typ, fld)

          setTargets []
          addGatewayModule modFile1 mod1
          addGatewayModule modFile2 mod2
          addGatewayModule modFile3 mod3

          -- Set targets log
          liftIO $ putStrLn $ "Done with setting up targets for " ++ show (typ, fld)

          !transformed <- performCommand builtinRefactorings (splitOn " " command)
                                          (Right ((SourceFileKey (moduleSourceFile mod1) mod1), sourced))
                                          []

          -- Tranform the module log
          liftIO $ putStrLn $ "Done with transforming module for " ++ show (typ, fld)

          case transformed of
            Right changes -> do
              forM changes $ \case
                ContentChanged (mod, correctlyTransformed) -> do
                  liftIO $ putStrLn $ "=========== transformed AST (" ++ (mod ^. sfkModuleName) ++ "):"
                  -- liftIO $ putStrLn $ srcInfoDebug correctlyTransformed

                  liftIO $ putStrLn $ "=========== transformed & prettyprinted (" ++ (mod ^. sfkModuleName) ++ "):"
                  let prettyPrinted = prettyPrint correctlyTransformed
                  -- liftIO $ putStrLn prettyPrinted

                  liftIO $ putStrLn $ "=========== Write into file (" ++ (mod ^. sfkModuleName) ++ "):"
                  let (origCont, newCont) = getContentsFromModules correctlyTransformed sourced
                  liftIO $ writeToFile modDir modFile1 newCont

                  -- Wrote transformed module log
                  liftIO $ putStrLn $ "Done with writing transformed module for " ++ show (typ, fld)
                  
                  liftIO $ putStrLn $ "=========== Load All Targets (" ++ (mod ^. sfkModuleName) ++ "):"
                  loadAllTargets

                  -- Loaded targets log
                  liftIO $ putStrLn $ "Done with loading all targets for " ++ show (typ, fld)

                  liftIO $ putStrLn $ "=========== Validate Changes :"
                  res <- validateChanges [mod2, mod3] [modFile2, modFile3]
                  liftIO $ putStrLn $ "Typecheck result - " ++ show res
                  if res then return (typ, fld, False) else return (typ, fld, True)

                ModuleRemoved mod -> do
                  liftIO $ putStrLn $ "=========== module removed: " ++ mod
                  return (typ, fld, True)

                ModuleCreated mod cont _ -> do
                  liftIO $ putStrLn $ "=========== created AST (" ++ mod ++ "):"
                  -- liftIO $ putStrLn $ srcInfoDebug cont

                  liftIO $ putStrLn $ "=========== created & prettyprinted (" ++ mod ++ "):"
                  -- let prettyPrinted = prettyPrint cont
                  -- liftIO $ putStrLn prettyPrinted

                  return (typ, fld, True) 

            Left transformProblem -> do
              liftIO $ putStrLn "==========="
              liftIO $ putStrLn transformProblem
              liftIO $ putStrLn "==========="
              return [(typ, fld, True)]

        -- Ran Typechecking log
        liftIO $ putStrLn $ "Done with typechecking for " ++ show (typ, fld)

        -- Delete temp directory
        -- liftIO $ removeDirectoryRecursive modDir

        -- deleted directory log
        -- liftIO $ putStrLn $ "Done with deleting temp directory for " ++ show (typ, fld)

        -- Log the result
        liftIO $ putStrLn $ "Whether used field (" ++ show typ ++ ", " ++ show fld ++ ") -> " ++ show result

        -- Update the cached fields result
        liftIO $ updateCachedFieldsFile gatewayName result `catch` (\(e :: SomeException) -> putStrLn ("Exception during cacheUpdate :: " ++ show e))

        -- Updated cache log
        liftIO $ putStrLn $ "Done with updating cache file for " ++ show (typ, fld)

        -- return result
        pure result 
      )
     
    liftIO $ putStrLn $ "Overall fields Map = " ++ show fieldsMap
    
    let recTypesNames = fmap fst recResTypes'
    let ([umod] :: [HT.Module]) = sourced ^? biplateRef 
    (depMap :: DepMap) <- getAllTypesDepsMap umod recTypesNames
    liftIO $ putStrLn $ ""
    liftIO $ putStrLn $ "Dependency Map = " ++ show depMap

    (diffMap :: DiffMap) <- pure $ getDistinguishableMap umod depMap fieldsMap
    liftIO $ putStrLn $ ""
    liftIO $ putStrLn $ "Distinguishable Map = " ++ show diffMap

    -- Delete temp directory
    liftIO $ removeDirectoryRecursive $ workingDir ++ "temp/"

    -- deleted directory log
    liftIO $ putStrLn $ "Done with deleting temp directory recursively"

    finalModule <- foldrM (\(typ, fld, isUsed) source -> do 
        if isUsed
          then return source
          else let command = "gw " ++ typ ++ " " ++ fld in applyMaybe source command moduleName -- apply refactoring
      ) sourced fieldsMap 
    
    liftIO $ putStrLn $ prettyPrint finalModule
      
applyMaybe sourced command moduleName = do 
  transformed <- performCommand builtinRefactorings (splitOn " " command)
                                (Right ((SourceFileKey (moduleSourceFile moduleName) moduleName), sourced))
                                []
  case transformed of
    Right [change] -> do
      case change of
        ContentChanged (mod, correctlyTransformed) -> do
          -- liftIO $ putStrLn $ "=========== transformed AST (" ++ (mod ^. sfkModuleName) ++ "):"
          -- liftIO $ putStrLn $ srcInfoDebug correctlyTransformed
          -- liftIO $ putStrLn $ "=========== transformed & prettyprinted (" ++ (mod ^. sfkModuleName) ++ "):"
          -- let prettyPrinted = prettyPrint correctlyTransformed
          -- liftIO $ putStrLn prettyPrinted
          return correctlyTransformed
        ModuleRemoved mod -> do
          liftIO $ putStrLn $ "=========== module removed: " ++ mod
          return sourced
        ModuleCreated mod cont _ -> do
          liftIO $ putStrLn $ "=========== created AST (" ++ mod ++ "):"
          -- liftIO $ putStrLn $ srcInfoDebug cont
          liftIO $ putStrLn $ "=========== created & prettyprinted (" ++ mod ++ "):"
          -- let prettyPrinted = prettyPrint cont
          -- liftIO $ putStrLn prettyPrinted
          return cont 
    Right _ -> return sourced
    Left transformProblem -> do
      liftIO $ putStrLn "==========="
      liftIO $ putStrLn transformProblem
      liftIO $ putStrLn "==========="
      return sourced

recTypesOnly :: Decl -> [(String, String)] -> [(String, String)]
recTypesOnly (DataDecl _ _ (NameDeclHead name) (AnnList [RecordConDecl _ fields]) _) r = 
  let typeName = showName name 
      constructorFields = getFieldsFromList fields
  in (fmap (\x -> (typeName, x)) constructorFields) ++ r
  where 
        getFieldsFromList :: FieldDeclList -> [String]
        getFieldsFromList (AnnList ls) = foldr getOptionalFieldsFromList [] ls 

        getOptionalFieldsFromList :: FieldDecl -> [String] -> [String]
        getOptionalFieldsFromList (FieldDecl (AnnList names) typ) r =
          let ((typName:_) :: [String]) = fmap showName $ typ ^? biplateRef 
              (fieldNames :: [String]) = fmap showName names 
          in if typName == "Maybe" then r 
             else fieldNames ++ r
        getOptionalFieldsFromList _ r = r 
recTypesOnly _ r = r 

-- Function write the transformation changes
applyChanges cmod mod tdir file = do
          let m = cmod 
              n = mod
              diffMode = False
          setCurrentDirectory tdir
          let newCont = prettyPrint m
              -- file = n ^. sfkFileName
          origCont <- liftIO $ withBinaryFile file ReadMode $ \handle -> do
            hSetEncoding handle utf8
            StrictIO.hGetContents handle
          let undo = createUndo 0 $ getGroupedDiff origCont newCont
          let unifiedDiff = createUnifiedDiff file origCont newCont
          when (not diffMode) $ do
            liftIO $ withBinaryFile file WriteMode $ \handle -> do
              hSetEncoding handle utf8
              hPutStr handle newCont
              hFlush handle
          return (undo, unifiedDiff, origCont)

-- Function to get the contents from the modules
getContentsFromModules cmod mod =
  let newCont = prettyPrint cmod
      origCont = prettyPrint mod 
  in (origCont, newCont)

-- Function write the transformation changes in memory only
applyChanges' cmod target = do
          let newCont = stringToStringBuffer $ prettyPrint cmod
          utcTime <- liftIO $ getCurrentTime
          removeTarget $ targetId target 
          let target' = target {targetContents = Just (newCont, utcTime)}
          addTarget target'

-- function to write the changed code to the file
writeToFile tdir file str = do 
  setCurrentDirectory tdir
  liftIO $ withBinaryFile file WriteMode $ \handle -> do
              hSetEncoding handle utf8
              hPutStr handle str
              hFlush handle
  return ()

-- | Creates a compressed set of changes in one file
createUndo :: Eq a => Int -> [Diff [a]] -> [(Int, Int, [a])]
createUndo i (Both str _ : rest) = createUndo (i + length str) rest
createUndo i (First rem : Second add : rest)
  = (i, i + length add, rem) : createUndo (i + length add) rest
createUndo i (First rem : rest) = (i, i, rem) : createUndo i rest
createUndo i (Second add : rest)
  = (i, i + length add, []) : createUndo (i + length add) rest
createUndo _ [] = []

-- Returns a pair of boolean (isInCached, cachedRes)
isCachedField :: (String, String) -> FieldsMap -> (Bool, Bool)
isCachedField _ [] = (False, False)
isCachedField field@(typeName, fieldName) ((typeName', fieldName', res) : rem) = 
  if typeName == typeName' && fieldName == fieldName' 
    then (True, res)
    else isCachedField field rem

getCachedFieldsMap :: String -> IO FieldsMap
getCachedFieldsMap gatewayName = do 
    cachedFieldsMapStr <- withBinaryFile ("/Users/piyush.garg/Desktop/CachedFields_" ++ gatewayName ++ ".txt") ReadWriteMode $ \handle -> do
                                                                            hSetEncoding handle utf8
                                                                            BS.hGetContents handle
    let (cachedFieldsMap :: FieldsMap) = case Aeson.decodeStrict (cachedFieldsMapStr) of
                                Just res -> res 
                                Nothing  -> trace ("Unable to decode") [] 
    return cachedFieldsMap           

-- Update the fieldsMap in the file after updating the values if already exist
updateCachedFieldsFile :: String -> FieldsMap -> IO ()
updateCachedFieldsFile gatewayName fieldsMap = do 
  cachedFieldsMap <- getCachedFieldsMap gatewayName
  let updatedFieldsMap = updateFieldsMap cachedFieldsMap fieldsMap
  withBinaryFile ("/Users/piyush.garg/Desktop/CachedFields_" ++ gatewayName ++ ".txt") WriteMode $ \handle -> do
                                                                            hSetEncoding handle utf8
                                                                            BSL.hPut handle (Aeson.encode updatedFieldsMap) 

-- Function to update the cachedFieldsMap as per the newFieldsMap
updateFieldsMap :: FieldsMap -> FieldsMap -> FieldsMap
updateFieldsMap oldFieldsMap newFieldsMap = foldr addUpdateIfNotExist oldFieldsMap newFieldsMap
  where 
     addUpdateIfNotExist :: (String, String, Bool) -> FieldsMap -> FieldsMap 
     addUpdateIfNotExist field@(typeName, fieldName, isUsed) [] = [field] 
     addUpdateIfNotExist field@(typeName, fieldName, _) (field'@(typeName2, fieldName2, _) : xs) = 
      if typeName == typeName2 && fieldName == fieldName2
        then field  : xs 
        else field' : addUpdateIfNotExist field xs
      -- if ((typeName, fieldName, True) `elem` res) || ((typeName, fieldName, False) `elem` res)
      --   then updateVal field res
      --   else field : res 

-- | Creates a unified-style diff of two texts. Only used when the user wants to know what would change.
createUnifiedDiff :: FilePath -> String -> String -> String
createUnifiedDiff name left right
  = render $ prettyContextDiff (PP.text name) (PP.text name) PP.text $ getContextDiff 3 (lines left) (lines right)

deriving instance Generic SrcSpan
deriving instance Generic (NodeInfo sema src)
instance Show AvailInfo where show = showSDocUnsafe . ppr

forcedTypecheck :: ModSummary -> ParsedModule -> Ghc (Maybe RenamedSource, TypecheckedSource)
forcedTypecheck ms p = do
  env <- getSession
  store <- liftIO $ newIORef (error "not found")
  let hpm = HsParsedModule (pm_parsed_source p) (pm_extra_src_files p) (pm_annotations p)
  tcRes <- liftIO $ runTcInteractive env $ (,) <$> getGblEnv <*> getLclEnv
  case tcRes of
    (_, Just (gblEnv, lclEnv)) -> do
      let finalizeModule = do gbl <- getGblEnv
                              liftIO $ writeIORef store ( (,,,) <$> tcg_rn_decls gbl
                                                                <*> return (tcg_rn_imports gbl)
                                                                <*> return (tcg_rn_exports gbl)
                                                                <*> return (tcg_doc_hdr gbl)
                                                        , tcg_binds gbl)
      -- liftIO $ modifyIORef (tcg_th_modfinalizers gblEnv) (finalizeModule :)
      let gblEnv' = gblEnv { tcg_rn_exports = Just [], tcg_rn_decls = Just emptyRnGroup }
      liftIO $ initTcRnIf 'a' env gblEnv' lclEnv $ void (tcRnModuleTcRnM env ms hpm (ms_mod ms, getLoc (pm_parsed_source p)))
                                                     `gcatch` \(_ :: SomeException) -> return ()
      liftIO $ readIORef store
    _ -> error "forcedTypecheck: runTcInteractive failed" 


addGatewayModule :: FilePath -> ModuleName -> Ghc Target
addGatewayModule filePath moduleName
  = do 
       target <- guessTarget filePath Nothing
       addTarget target
       return target

loadAllTargets :: Ghc ()
loadAllTargets = do 
  void $ load LoadAllTargets
  return ()

loadModSummaries :: Ghc ()
loadModSummaries = depanal [] False >> return ()

-- | Load the summary of a module given by the working directory and module name.
loadGatewayModule :: FilePath -> ModuleName -> Ghc ModSummary
loadGatewayModule filePath moduleName
  = do 
       target <- guessTarget filePath Nothing
       addTarget target
       void $ load (LoadUpTo $ GHC.mkModuleName moduleName)
       targets <- getTargets
       liftIO $ print ("Target = " ++ (show' targets))
       getModSummary $ GHC.mkModuleName moduleName

initGhcFlagsForGateway :: Ghc ()
initGhcFlagsForGateway = do 
                         initGhcFlagsForGateway' False True
                         dfs <- getSessionDynFlags
                         void $ setSessionDynFlags $ dfs { hscTarget = HscAsm
                                                        --  , pluginModNames = pluginModNames dfs ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
                                                          }

setTempFilesFlags :: Bool -> Ghc ()
setTempFilesFlags shouldSet = do 
  dflags <- getSessionDynFlags
  let setOrUnset = if shouldSet then gopt_set else gopt_unset
  void $ setSessionDynFlags
    $ flip setOrUnset Opt_KeepHcFiles
    $ flip setOrUnset Opt_KeepHscppFiles
    $ flip setOrUnset Opt_KeepSFiles
    $ flip setOrUnset Opt_KeepTmpFiles
    $ flip setOrUnset Opt_KeepHiFiles
    $ flip setOrUnset Opt_KeepOFiles
    $ dflags

-- | Sets up basic flags and settings for GHC
initGhcFlagsForGateway' :: Bool -> Bool -> Ghc ()
initGhcFlagsForGateway' needsCodeGen errorsSuppressed = do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ flip gopt_set Opt_BreakOnError
    $ flip gopt_set Opt_BreakOnException
    $ flip gopt_unset Opt_WriteInterface
    $ flip gopt_unset Opt_WriteHie
    $ (if errorsSuppressed then
                                    flip gopt_set Opt_DeferTypedHoles
                                  . flip gopt_set Opt_DeferTypeErrors
                                  . flip gopt_set Opt_DeferOutOfScopeVariables
                           else id)
    $ dflags { importPaths = []
            --  , maxErrors = Nothing
             , parMakeCount = Nothing
             , hscTarget = if needsCodeGen then HscInterpreted else HscNothing
             , ghcLink = if needsCodeGen then LinkInMemory else NoLink
             , ghcMode = CompManager
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
            --  , pluginModNames = pluginModNames dflags ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
             }

validateChanges :: [String] -> [FilePath] -> Ghc Bool 
validateChanges [] [] = pure True
validateChanges (modName : modNames) (file : files) = do
  liftIO $ putStrLn $ "Typecheck " ++ modName 
  isTypeChecked <- typeCheck (modName, file)
  if isTypeChecked
    then validateChanges modNames files 
    else pure False 

typeCheck :: (String, FilePath) -> Ghc Bool
typeCheck (moduleName, modFile) = do
    -- Debugging prints
    flags <- getSessionDynFlags
    liftIO $ print ("Import Paths :: " ++ (show $ importPaths flags))
    liftIO $ print ("Load file :: " ++ modFile ++ " , Module :: " ++ moduleName)

    -- Load module
    -- ms <- loadGatewayModule modFile moduleName
    ms <- getModSummary $ GHC.mkModuleName moduleName

    -- Update session Dyn Flags for plugins
    flags <- return $ ms_hspp_opts ms
      
    p' <- withTempSession (addPluginFlags flags) $
      do 
        -- Initialize plugins
        dflags <- getSessionDynFlags
        hsc_env <- getSession
        dflags <- liftIO (initializePlugins hsc_env dflags)
        ms' <- pure $ modSumNormalizeFlags $ ms { ms_hspp_opts = dflags }

        p' <- parseModule ms'
        return p'
    
    ((\_ -> True) <$> typecheckModule p')
                         `gcatch` \(e :: SourceError) -> trace ("Some exception : " ++ show e) (return False)

clearModules :: [ModSummary] -> Ghc ()
clearModules [] = return ()
clearModules mods = do
  let reachableMods = map ms_mod_name mods
      notReloaded = (`notElem` reachableMods) . GHC.moduleName . mi_module . hm_iface
  env <- getSession
  let hptStay = filterHpt notReloaded (hsc_HPT env)
  -- clear the symbol cache for iserv
  liftIO $ purgeLookupSymbolCache env
  -- clear the global linker state
  dfs <- getSessionDynFlags
  when (not $ gopt Opt_ExternalInterpreter dfs) $
    liftIO $ unload env (mapMaybe hm_linkable (eltsHpt hptStay))
  -- clear name cache
  nameCache <- liftIO $ readIORef $ hsc_NC env
  let nameCache' = nameCache { nsNames = delModuleEnvList (nsNames nameCache) (map ms_mod mods) }
  liftIO $ writeIORef (hsc_NC env) nameCache'
  -- clear home package table and module graph
  modifySession (\s -> s { hsc_HPT = hptStay
                                , hsc_mod_graph = mkModuleGraph $ filter ((`notElem` reachableMods) . ms_mod_name) (mgModSummaries $ hsc_mod_graph s)
                                })
  ss <- getSession
  liftIO $ flushFinderCaches ss

getAllTypesDepsMap :: HT.Module -> [String] -> Ghc DepMap
getAllTypesDepsMap source recTypesNames = do
  let (dataDecls :: [Decl]) = source ^? biplateRef
      (conTypes :: [Decl]) = filter conDeclTypeHavingRecTypes dataDecls
      (conTypesNames :: [String]) = fmap (\(DataDecl _ _ (NameDeclHead name) _ _) -> showName name) conTypes
      (conDecls :: [HT.ConDecl]) = conTypes ^? biplateRef
      conNameMap = Map.fromList $ foldr getConNameTypeMap [] conDecls
      fromJSONInstances = getJSONInstances conTypesNames
      orderedListFromInstancesConst = getOrderedListFromInstances fromJSONInstances conNameMap False
      orderedListFromInstances = getOrderedListFromInstances fromJSONInstances conNameMap True
      (depMap :: DepMap) = orderedListToMap orderedListFromInstances
  !_ <- liftIO $ putStrLn ""
  !_ <- liftIO $ putStrLn $ "Con Data Type Names = " ++ show conTypesNames
  !_ <- liftIO $ putStrLn ""
  !_ <- liftIO $ putStrLn $ "Con Name-Type Map   = " ++ show conNameMap
  -- !_ <- liftIO $ putStrLn ""
  -- !_ <- liftIO $ putStrLn "" >> putStrLn $ "Instance decls = " ++ foldr (\x r -> srcInfoDebug x ++ r) "" fromJSONInstances
  !_ <- liftIO $ putStrLn ""
  !_ <- liftIO $ putStrLn $ "Instance decls ordered names list (Constructor Names) = " ++ show orderedListFromInstancesConst
  !_ <- liftIO $ putStrLn ""
  !_ <- liftIO $ putStrLn $ "Instance decls ordered names list (Types Names) = " ++ show orderedListFromInstances
  return depMap

  where 
    conDeclTypeHavingRecTypes :: Decl -> Bool 
    conDeclTypeHavingRecTypes (DataDecl _ _ _ (AnnList cons) _) = any checkInRecTypesNames cons 
    conDeclTypeHavingRecTypes _ = False 

    checkInRecTypesNames :: HT.ConDecl -> Bool 
    checkInRecTypesNames (ConDecl name (AnnList types)) = any checkTypeInRecTypeNames types
    checkInRecTypesNames _ = False

    checkTypeInRecTypeNames :: HT.Type -> Bool 
    checkTypeInRecTypeNames typ = let (names :: [HT.Name]) = typ ^? biplateRef in any (\n -> (showName n) `elem` recTypesNames) names

    getConNameTypeMap :: HT.ConDecl -> [(String, String)] -> [(String, String)]
    getConNameTypeMap (ConDecl name (AnnList types)) res = 
      if length types <= 0
      then res
      else 
        let conName = showName name 
            (typeNames :: [HT.Name]) = (head types) ^? biplateRef
            conType = (showName . head) typeNames
        in (conName, conType) : res

    getJSONInstances :: [String] -> [Decl]
    getJSONInstances dataNames = filter (isJust . getInstanceDecl dataNames) (source ^? biplateRef)

    getInstanceDecl :: [String] -> Decl -> Maybe Decl 
    getInstanceDecl dataNames decl@(InstanceDecl (InstanceRule _ _ (AppInstanceHead fun arg)) body) = 
      if (showName $ head $ fun ^? biplateRef) == "FromJSON" && (showName $ head $ arg ^? biplateRef) `elem` dataNames
      then Just decl 
      else Nothing
    getInstanceDecl _ _ = Nothing

    getOrderedListFromInstances :: [Decl] -> ConNameMap -> Bool -> [[String]]
    getOrderedListFromInstances instances conNameMap convert = fmap (getNamesFromInstances conNameMap convert) instances

    getNamesFromInstances :: ConNameMap -> Bool -> Decl -> [String]
    getNamesFromInstances conNameMap convert (InstanceDecl _ body) =
      if convert
      then fmap (fromJust . flip Map.lookup conNameMap . showName) $ filter (\x -> isJust $ Map.lookup (showName x) conNameMap) (body ^? biplateRef)
      else fmap showName $ filter (\x -> isJust $ Map.lookup (showName x) conNameMap) (body ^? biplateRef)

orderedListToMap :: [[String]] -> DepMap 
orderedListToMap = foldr lsToMap mempty
  where 
    lsToMap :: [String] -> DepMap -> DepMap
    lsToMap (x : xs) depMap = Map.insertWith (++) x xs $ lsToMap xs depMap
    lsToMap [] depMap = depMap 

addPluginFlags :: DynFlags -> HscEnv -> HscEnv 
addPluginFlags flags env = let dflags = hsc_dflags env in 
                    env {hsc_dflags = dflags {pluginModNames = pluginModNames flags, pluginModNameOpts = pluginModNameOpts flags}}

isUsedField :: (String, String, String) -> [(String, String, Bool)] -> Bool 
isUsedField _ [] = False 
isUsedField field@(typeName, fieldName, fieldType) ((typ, fld, res) : xs) = if typeName == typ && fieldName == fld then res else isUsedField field xs

canDistinguish :: HT.Module -> FieldsMap -> String -> String -> DiffMap -> DiffMap
canDistinguish source fieldsMap typeA typeB res = 
  let recTypes = getRecordTypes source in
    if typeA `elem` recTypes && typeB `elem` recTypes -- if not primitive types
      then 
        let typeA' = getDeclFromName source typeA -- get from source the decl with typeA name 
            typeB' = getDeclFromName source typeB -- get from source the decl with typeB name 
        in 
          if checkCriteriaOne typeA' typeB' fieldsMap -- if extra *REQUIRED* field is there in typeA
            then Map.insertWith (&&) typeA True res -- then this type can be distinguished as of now
            else -- now this type can have either same fields with different types or fields without any distinctor, these cases are needed to be logged
              let 
                emptyMap = Map.singleton typeA False 
                tempRes = getCriteriaTwo source fieldsMap typeA' typeB' emptyMap -- use canDistinguish for all the fields present in both 
              in Map.unionWith (&&) tempRes res
    else res -- if primitive types

checkCriteriaOne :: Decl -> Decl -> FieldsMap ->  Bool 
checkCriteriaOne typeA typeB fieldsMap = 
  let fieldsA     = getFieldsFromDecl typeA
      fieldsB     = getFieldsFromDecl typeB
      fieldsOnlyA = snd3 <$> fieldsA
      fieldsOnlyB = snd3 <$> fieldsB
  in any (notInBAndIsUsed fieldsOnlyB) fieldsA-- get fields and check if any extra field is there which is usedField
    where 
      notInBAndIsUsed :: [String] -> (String, String, String) -> Bool 
      notInBAndIsUsed fieldsOnlyB field@(_, fieldName, _) = (fieldName `notElem` fieldsOnlyB) && isUsedField field fieldsMap

getCriteriaTwo :: HT.Module -> FieldsMap -> Decl -> Decl -> DiffMap -> DiffMap 
getCriteriaTwo source fieldsMap typeA typeB diffMap = 
  let fieldsA     = getFieldsFromDecl typeA
      fieldsB     = getFieldsFromDecl typeB
      fieldsOnlyA = snd3 <$> fieldsA
      fieldsOnlyB = snd3 <$> fieldsB
  in foldr (isInBAndHasDistinguishableType fieldsA fieldsB fieldsOnlyB) diffMap fieldsOnlyA
    where 
      isInBAndHasDistinguishableType :: [(String, String, String)] -> [(String, String, String)] -> [String] -> String -> DiffMap -> DiffMap
      isInBAndHasDistinguishableType fieldsA fieldsB fieldsOnlyB fieldName res = 
        if fieldName `elem` fieldsOnlyB
          then canDistinguish source fieldsMap (getFieldType fieldsA fieldName) (getFieldType fieldsB fieldName) res
        else res

      getFieldType :: [(String, String, String)] -> String -> String
      getFieldType [] _ = ""
      getFieldType ((typeName, fieldName, fieldType) : xs) field = if fieldName == field then fieldType else getFieldType xs field

getDistinguishableMap :: HT.Module -> DepMap -> FieldsMap -> DiffMap
getDistinguishableMap source depMap fieldsMap = Map.foldrWithKey evaluateDistinguishablePrimary mempty depMap
  where 
    evaluateDistinguishablePrimary :: String -> [String] -> DiffMap -> DiffMap 
    evaluateDistinguishablePrimary typeName deps res = 
      let
        emptyMap = Map.singleton typeName True
        tempRes = foldr (canDistinguish source fieldsMap typeName) emptyMap deps 
      in trace ("evaluateDistinguishablePrimary :: " ++ typeName ++ " with deps " ++ show deps ++ " with res = " ++ show tempRes) $ Map.unionWith (&&) tempRes res

getDeclFromName :: HT.Module -> String -> Decl 
getDeclFromName source name = 
  let (decls :: [Decl]) = source ^? biplateRef
  in head $ filter (\x -> getDeclName x == name) decls 

getDeclName :: Decl -> String 
getDeclName (DataDecl _ _ (NameDeclHead name) _ _) = showName name 
getDeclName _ = ""

getFieldsFromDecl :: Decl -> [(String, String, String)] -- returns (typeName, fieldName, fieldType)
getFieldsFromDecl (DataDecl _ _ (NameDeclHead name) (AnnList [RecordConDecl _ (AnnList ls)]) _) = fmap (\(x,y) -> (showName name, x, y)) $ foldr (\x r -> getFieldsFromFieldDecl x ++ r) [] ls
  where 
        getFieldsFromFieldDecl :: FieldDecl -> [(String, String)]
        getFieldsFromFieldDecl (FieldDecl (AnnList names) typ) =
          let ((typName:_) :: [String]) = fmap showName $ typ ^? biplateRef 
              (fieldNames :: [String]) = fmap showName names 
          in fmap (,typName) fieldNames 
        getOptionalFieldsFromList _ = []
getFieldsFromDecl _ = []

getRecordTypes :: HT.Module -> [String]
getRecordTypes source = 
  let (decls :: [Decl]) = source ^? biplateRef
      (recordDecls :: [Decl]) = filter isRecordType decls
  in getDeclName <$> recordDecls

isRecordType :: Decl -> Bool 
isRecordType (DataDecl _ _ (NameDeclHead _) (AnnList [RecordConDecl _ _]) _) = True 
isRecordType _ = False
