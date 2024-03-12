{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Tools.Debug where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO(..))
import Control.Reference ((^.), biplateRef, (^?), (!~))
import Data.List.Split (splitOn)
import Data.Maybe (Maybe(..), fromJust)
import GHC.Generics (Generic(..))
import System.FilePath (pathSeparator, (</>), (<.>))
import Data.List (isInfixOf, find)
import FastString


import GHC hiding (loadModule, ModuleName, DataDecl)
import GHC.Paths ( libdir )
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import StringBuffer (hGetStringBuffer, stringToStringBuffer)
import Outputable
import HscTypes
import qualified HsDecls as GHC
import TcRnDriver
import TcRnTypes
import TcRnMonad
import Data.IORef
import DynFlags
import Avail
import SrcLoc
import GHC.LanguageExtensions as GHC

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
import EnumSet (toList, empty)
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
import HeaderInfo

import Data.Time.Clock (getCurrentTime)

import Control.Concurrent.Async
import UnliftIO.Async
import Control.Concurrent
import System.Directory (copyFile)

import qualified Data.Aeson as Aeson

import System.Environment (lookupEnv, getEnvironment)
import Text.Read (readMaybe)

import System.Mem (performGC, performMajorGC, performMinorGC)

import Data.List (sort)

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
    _ <- useDirs [(head $ splitOn "/src-generated" workingDir) ++ "/src", (head $ splitOn "/src-generated" workingDir) ++ "/src-extras", (head $ splitOn "/src-generated" workingDir) ++ "/src-generated", workingDir, "/Users/piyush.garg/ht-gateway-task/euler-api-txns/dist-newstyle/build/x86_64-osx/ghc-8.8.4/euler-api-txns-1.0.0/x/euler-api-txns/build/euler-api-txns/autogen"]
    _ <- useFlags args
    !_ <- setTempFilesFlags False

    -- create temp directory & set temp directory
    liftIO $ createDirectoryIfMissing False (workingDir ++ "/temp")
    !oldFlags <- getSessionDynFlags
    !_ <- setSessionDynFlags $ setTmpDir (workingDir ++ "/temp") oldFlags
    !_ <- setIntermediateOutputDirs (workingDir ++ "/temp")

    -- Create moduleNames for loading and fileNames for target
    let modFileNames = ["Types", "Transforms", "Flow"]
        modNames = fmap (\x -> "Gateway." ++ gatewayName ++ "." ++ x) modFileNames
        moduleName = modNames !! 0
        modFile = workingDir ++ (modFileNames !! 0) ++ ".hs"
    origModContent <- liftIO $ getRawContentFromModule modFile

    -- let filePath = workingDir ++ "Env.hs"
    -- target <- guessTarget filePath Nothing
    -- setTargets [target]

    -- Debugging prints
    liftIO $ print $ ("Module Names :: " ++ show modNames)
    flags <- getSessionDynFlags
    liftIO $ print ("Import Paths :: " ++ (show $ importPaths flags))
    liftIO $ print ("Load file :: " ++ modFile ++ " , Module :: " ++ moduleName)

    -- Load module and mod summary
    liftIO $ putStrLn "=========== parsed source:"
    (ms, target1) <- loadGatewayModule modFile moduleName
    !(_, target2) <- loadGatewayModule (workingDir ++ "Transforms.hs") ("Gateway." ++ gatewayName ++ ".Transforms")
    !(_, target3) <- loadGatewayModule (workingDir ++ "Flow.hs") ("Gateway." ++ gatewayName ++ ".Flow")
    p <- parseModule ms
    let annots = pm_annotations $ p
        ms = pm_mod_summary p

    -- headers <- liftIO $ getOptionsFromFile (ms_hspp_opts ms) modFile
    -- liftIO $ putStrLn $ show $ fmap unLoc headers
    -- liftIO $ putStrLn $ show' $ pm_parsed_source p -- pretty prints the module
    -- liftIO $ putStrLn $ showAst $ pm_parsed_source p -- shows the AST
    -- let (mydecls :: [TyClDecl GhcPs]) = (pm_parsed_source p) ^? biplateRef

    -- let 
    --   updateNewTypeToData :: TyClDecl GhcPs -> Ghc (TyClDecl GhcPs)
    --   updateNewTypeToData decl = case decl of 
    --                                 (GHC.DataDecl a1 name vars a2 (HsDataDefn a3 NewType ctx a4 kind cons derivs)) -> return $ (GHC.DataDecl a1 name vars a2 (HsDataDefn a3 DataType ctx a4 kind cons derivs))
    --                                 x -> return x
    -- modAst <- (!~) (biplateRef) (updateNewTypeToData) (pm_parsed_source p)
    -- let (mydecls :: [TyClDecl GhcPs]) = (modAst) ^? biplateRef
    --     newtypeDeclFilter decl = case decl of 
    --                                 (GHC.DataDecl _ name vars _ (HsDataDefn _ NewType ctx _ kind cons derivs)) -> True
    --                                 _ -> False
    --     (newtypeDecls :: [TyClDecl GhcPs]) = filter newtypeDeclFilter mydecls
    -- liftIO $ putStrLn $ "New NewTypeDecls = " ++ show' newtypeDecls

    -- let (mydecls :: [TyClDecl GhcPs]) = (pm_parsed_source p) ^? biplateRef
    --     newtypeDeclFilter decl = case decl of 
    --                                 (GHC.DataDecl _ name vars _ (HsDataDefn _ NewType ctx _ kind cons derivs)) -> True
    --                                 _ -> False
    --     (newtypeDecls :: [TyClDecl GhcPs]) = filter newtypeDeclFilter mydecls
    -- liftIO $ putStrLn $ "Old NewTypeDecls = " ++ show' newtypeDecls

    -- let (mydecls :: [TyClDecl GhcPs]) = (pm_parsed_source p) ^? biplateRef
    --     newtypeDeclFilter decl = case decl of 
    --                                     (GHC.DataDecl _ name vars _ (HsDataDefn _ NewType ctx _ kind cons derivs)) -> True
    --                                     _ -> False
    --     (newtypeDecls :: [TyClDecl GhcPs]) = filter newtypeDeclFilter mydecls
    --     spans = fmap getKeywordLoc newtypeDecls
    --     checkAndMkReplacement sp = case sp of
    --                                 Left _ -> mkEmptyReplacement
    --                                 Right sp -> mkReplacement sp "data"
    --     replacements = fmap checkAndMkReplacement spans
    -- liftIO $ putStrLn $ "Old NewTypeDecls Locations = " ++ show spans
    -- liftIO $ putStrLn $ "Replacements = " ++ show replacements

    -- Perform the refactoring in the code
    -- liftIO $ replaceInFile replacements modFile

    -- liftIO $ putStrLn $ show' $ hsmodDecls $ unLoc $ pm_parsed_source p

    -- liftIO $ putStrLn $ "Updated module " 
    -- liftIO $ putStrLn $ show' modAst 
    -- liftIO $ writeToFile workingDir "Types.hs" $ show' modAst 

    let sourced = (pm_parsed_source p)
    let (dataDecls' :: [TyClDecl GhcPs]) = sourced ^? biplateRef
        (dataDecls :: [TyClDecl GhcPs]) = filter filterDataDeclsGHC dataDecls'
        (recTypes :: [(String, String)]) = foldr recTypesOnlyGHC [] dataDecls
        (recResTypes' :: [(String, String)]) = recTypes
        (recResTypes'' :: [(String, String)]) = take 10 recResTypes'

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

    cachedFieldsMap <- liftIO $ getCachedFieldsMap workingDir gatewayName
    liftIO $ putStrLn $ show cachedFieldsMap

    concurrentModules' <- liftIO $ lookupEnv "HT_GATEWAY_CON"
    let concurrentModules = maybe (Just 5) (readMaybe) concurrentModules'

    -- Generate fieldsMap concurrently
    -- (fieldsMap :: FieldsMap) <- liftIO $ concat <$> pooledForConcurrentlyN (fromJust concurrentModules) recResTypes'' (\(typ, fld) -> do
    (fieldsMap :: FieldsMap) <- concat <$> forM recResTypes'' (\(typ, fld) -> do
      -- Check if already cached result
      let (!isCached, !cacheRes) = isCachedField (typ, fld) cachedFieldsMap
      !_ <- liftIO $ putStrLn $ "Checked in cache"

      if False then do 
        !_ <- liftIO $ putStrLn $ "Found in Cache " ++ show (typ, fld, cacheRes)
        return [(typ, fld, cacheRes)]
      else do 
        !_ <- liftIO $ putStrLn $ "Not Found in Cache " ++ show (typ, fld)

        -- Create constants
        let !command  = "gw " ++ typ ++ " " ++ fld
            !modDir'  = workingDir ++ "temp/temp_" ++ typ ++ "_" ++ fld ++ "/"
            !modDir   = modDir' ++ "Gateway/" ++ gatewayName ++ "/" 
            -- !modDir = workingDir
            !modFile1 = modDir ++ "Types.hs"
            !modFile2 = modDir ++ "Transforms.hs"
            !modFile3 = modDir ++ "Flow.hs"
            !mod1     = "Gateway." ++ gatewayName ++ ".Types" 
            !mod2     = "Gateway." ++ gatewayName ++ ".Transforms" 
            !mod3     = "Gateway." ++ gatewayName ++ ".Flow" 

        -- liftIO $ putStrLn $ "Delete gateway temp = " ++ workingDir ++ "temp/" ++ "Gateway/" ++ gatewayName ++ "/"
        -- liftIO $ removeDirectoryRecursive $ workingDir ++ "temp/" ++ "Gateway/" ++ gatewayName ++ "/" 

        -- removeTarget $ (TargetModule $ GHC.mkModuleName $ "Gateway." ++ gatewayName ++ ".Types") 
        -- removeTarget $ (TargetModule $ GHC.mkModuleName $ "Gateway." ++ gatewayName ++ ".Flow") 
        -- removeTarget $ (TargetModule $ GHC.mkModuleName $ "Gateway." ++ gatewayName ++ ".Transform") 
        -- removeTarget $ targetId target1 
        -- removeTarget $ targetId target2 
        -- removeTarget $ targetId target3 

        -- Created constants log
        !_ <- liftIO $ putStrLn $ "Done with creating constants for " ++ show (typ, fld)

        -- create temp directory
        !_ <- liftIO $ createDirectoryIfMissing True modDir

        -- Created Directory log
        !_ <- liftIO $ putStrLn $ "Done with creating temp directory for " ++ show (typ, fld)

        -- copy files
        liftIO $ copyFile (workingDir ++ "Types.hs")      modFile1
        liftIO $ copyFile (workingDir ++ "Transforms.hs") modFile2
        liftIO $ copyFile (workingDir ++ "Flow.hs")       modFile3

        -- Copy finish log
        liftIO $ putStrLn $ "Done with copying the files for " ++ show (typ, fld)

        -- run a separate GHC session
        -- !result <- liftIO $ runGhc (Just libdir) $ do 
        -- !_ <- initGhcFlagsForGateway' False False
        -- !_ <- useDirs [modDir, (head $ splitOn "/src-generated" workingDir) ++ "/src-extras", (head $ splitOn "/src-generated" workingDir) ++ "/src-generated", workingDir, "/Users/piyush.garg/ht-gateway-task/euler-api-txns/dist-newstyle/build/x86_64-osx/ghc-8.8.4/euler-api-txns-1.0.0/x/euler-api-txns/build/euler-api-txns/autogen"]
        -- !_ <- useFlags args
        -- !_ <- setTempFilesFlags True
          
        -- !oldFlags <- getSessionDynFlags
        -- !_ <- setSessionDynFlags $ setTmpDir modDir oldFlags
        -- !_ <- setIntermediateOutputDirs modDir'
    
        -- !oldHsc <- getSession
        -- !oldFlags <- getSessionDynFlags
        -- !newHsc <- addPluginFlags 

        -- Set session log
        liftIO $ putStrLn $ "Done with setting up session for " ++ show (typ, fld)

        -- !transformed <- performCommand builtinRefactorings (splitOn " " command)
        --                                 (Right ((SourceFileKey (moduleSourceFile mod1) mod1), sourced))
        --                                 []

        let transformed = gw typ fld sourced
        
        -- Tranform the module log
        liftIO $ putStrLn $ "Done with transforming module for " ++ show (typ, fld)

        -- Wrote the changes log
        liftIO $ replaceInFile transformed modFile1
        liftIO $ putStrLn $ "Done with writing transformed module for " ++ show (typ, fld)        

        setTargets []
        target1 <- addGatewayModuleByPath modFile1 mod1
        target2 <- addGatewayModuleByPath modFile2 mod2
        target3 <- addGatewayModuleByPath modFile3 mod3
        -- addGatewayModule modDir' mod1
        -- addGatewayModule modDir' mod2
        -- addGatewayModule modDir' mod3
        -- setDirs [modDir']
        !_ <- setDirs [modDir, (head $ splitOn "/src-generated" workingDir) ++ "/src", (head $ splitOn "/src-generated" workingDir) ++ "/src-extras", (head $ splitOn "/src-generated" workingDir) ++ "/src-generated", workingDir, "/Users/piyush.garg/ht-gateway-task/euler-api-txns/dist-newstyle/build/x86_64-osx/ghc-8.8.4/euler-api-txns-1.0.0/x/euler-api-txns/build/euler-api-txns/autogen"]

        -- Set targets log
        liftIO $ putStrLn $ "Done with setting up targets for " ++ show (typ, fld)

        liftIO $ putStrLn $ "=========== Load All Targets for " ++ show (typ, fld)
        res <- loadAllTargets
        -- Loaded targets log
        liftIO $ putStrLn $ "Done with loading all targets for " ++ show (typ, fld)

        -- liftIO $ putStrLn $ "=========== Validate Changes :"
        -- let tempMods = [] -- ["Product.Gateway.Types", "Product.OLTP.CommonGatewayService", "EC.PreAuth", "VerifyIntegrityService", "EC.GatewayTxnData"]
        --     tempFiles = fmap (\x -> (head $ splitOn "Gateway" workingDir) ++ replaceDotWithSlash x) tempMods
        -- res <- validateChanges ([mod1] ++ tempMods ++ [mod2, mod3]) $ [modFile1] ++ tempFiles ++ [modFile2, modFile3]
        liftIO $ putStrLn $ "Typecheck result - " ++ show res
        !result <- if res then return [(typ, fld, False)] else return [(typ, fld, True)]

        -- Ran Typechecking log
        liftIO $ putStrLn $ "Done with typechecking for " ++ show (typ, fld)

        -- Delete temp directory
        -- liftIO $ removeDirectoryRecursive modDir

        -- deleted directory log
        -- liftIO $ putStrLn $ "Done with deleting temp directory for " ++ show (typ, fld)

        -- Log the result
        liftIO $ putStrLn $ "Whether used field (" ++ show typ ++ ", " ++ show fld ++ ") -> " ++ show result

        -- write back original file
        liftIO $ writeToFile workingDir "Types.hs" $ origModContent

        -- Update the cached fields result
        liftIO $ updateCachedFieldsFile workingDir gatewayName result `catch` (\(e :: SomeException) -> putStrLn ("Exception during cacheUpdate :: " ++ show e))

        -- Updated cache log
        liftIO $ putStrLn $ "Done with updating cache file for " ++ show (typ, fld)

        -- perform GC
        -- !_ <- liftIO performGC

        -- GC Done log
        -- liftIO $ putStrLn $ "Done with performing GC for " ++ show (typ, fld)

        -- return result
        pure result
      )
     
    liftIO $ putStrLn $ "Overall fields Map = " ++ show fieldsMap
    
    error "Stop here for now"

    let recTypesNames = fmap fst recResTypes'
    let ([umod] :: [HT.Module]) = [] --sourced ^? biplateRef 
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

    sourced <- pure undefined

    finalModule <- foldrM (\(typ, fld, isUsed) source -> do 
        if isUsed
          then (liftIO $ printLog "Used field : " (typ, fld)) >> return source
        else if isDistinguishable typ diffMap
          then let command = "gw " ++ typ ++ " " ++ fld in applyMaybe source command moduleName -- apply refactoring
        else 
          (liftIO $ printLog "Can't distinguish/decide from the type : " (typ, fld)) >> return source
      ) sourced fieldsMap 
    
    liftIO $ putStrLn $ prettyPrint finalModule

    -- Write finally to the file
    liftIO $ writeToFile workingDir "Types.hs" $ prettyPrint finalModule

    -- Wrote finally to file log
    liftIO $ putStrLn $ "Done with writing the final modification"

    -- perform GC
    !_ <- liftIO performGC

    -- GC Done log
    liftIO $ putStrLn $ "Done with performing final GC"


getKeywordLoc :: TyClDecl GhcPs -> Either String Span
getKeywordLoc (GHC.DataDecl _ (L (RealSrcSpan loc) _) _ _ _) = 
  let startLine = srcSpanStartLine loc 
      endLine   = srcSpanEndLine loc 
      startCol  = srcSpanStartCol loc 
      endCol    = srcSpanEndCol loc
    in
      if startLine == endLine && startCol <= endCol
        then Right $ mkSpan (mkLocation startLine 1) (mkLocation startLine $ startCol - 2)
        else Left "Data Declaration, but invalid locations"
getKeywordLoc _ = Left "Not a Data Declaration, Should not have reached here"


-- Function to Apply maybe refactoring over a particular type and field
applyMaybe :: UnnamedModule -> String -> String -> Ghc UnnamedModule
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

filterDataDeclsGHC :: TyClDecl GhcPs -> Bool
filterDataDeclsGHC (GHC.DataDecl _ (L _ typName) _ _ (HsDataDefn _ _ _ _ _ [L _ (ConDeclH98 { con_name = name, con_args = GHC.RecCon (unLoc -> fields)})] _)) = responseTypesOnly && notStandardResponse 
  where
    responseTypesOnly = let typ = show' typName in not (isInfixOf "req" typ || isInfixOf "Req" typ)
    notStandardResponse = 
      let (allFields :: [String]) = getFieldsListGHC fields
        in not (sort allFields == sort ["code", "status", "response"])
filterDataDeclsGHC _ = False

getFieldsListGHC :: [LConDeclField GhcPs] -> [String]
getFieldsListGHC fields = foldr getFields [] $ fmap unLoc fields
  where 
    getFields :: GHC.ConDeclField GhcPs -> [String] -> [String]
    getFields (ConDeclField _ names typ _) r = fmap (show' . unLoc) names ++ r
    getFields _ r = r 

recTypesOnlyGHC :: TyClDecl GhcPs -> [(String, String)] -> [(String, String)]
recTypesOnlyGHC (GHC.DataDecl _ (L _ typName) _ _ (HsDataDefn _ _ _ _ _ [L _ (ConDeclH98 { con_name = name, con_args = GHC.RecCon (unLoc -> fields)})] _)) r = 
  let typeName = show' typName 
      constructorFields = getFieldsFromList fields
  in (fmap (\x -> (typeName, x)) constructorFields) ++ r
  where 
        getFieldsFromList :: [LConDeclField GhcPs] -> [String]
        getFieldsFromList ls = foldr getOptionalFieldsFromList [] (fmap unLoc ls) 

        getOptionalFieldsFromList :: GHC.ConDeclField GhcPs -> [String] -> [String]
        getOptionalFieldsFromList (ConDeclField _ names typ _) r =
          let typName = show' $ unLoc typ 
              (fieldNames :: [String]) = fmap (show' . unLoc) names 
          in if (head $ words typName) == "Maybe" then r 
             else fieldNames ++ r
        getOptionalFieldsFromList _ r = r 
recTypesOnlyGHC _ r = r

filterDataDecls :: Decl -> Bool 
filterDataDecls (DataDecl _ _ (NameDeclHead name) (AnnList [RecordConDecl _ fields]) _) = responseTypesOnly && notStandardResponse 
  where 
    responseTypesOnly = let typ = showName name in not (isInfixOf "req" typ || isInfixOf "Req" typ)
    notStandardResponse = 
      let (allFields :: [String]) = getFieldsList fields
        in not (sort allFields == sort ["code", "status", "response"])
filterDataDecls _ = False

getFieldsList :: FieldDeclList -> [String]
getFieldsList (AnnList ls) = foldr getFields [] ls
  where 
    getFields :: FieldDecl -> [String] -> [String]
    getFields (FieldDecl (AnnList names) typ) r = fmap showName names ++ r
    getFields _ r = r 

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
applyChanges :: (SourceInfoTraversal node) => node dom SrcTemplateStage -> p -> FilePath -> FilePath -> IO ([(Int, Int, String)], String, String)
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
getContentsFromModules :: (SourceInfoTraversal node1, SourceInfoTraversal node2) => node1 dom1 SrcTemplateStage -> node2 dom2 SrcTemplateStage -> (String, String)
getContentsFromModules cmod mod =
  let newCont = prettyPrint cmod
      origCont = prettyPrint mod 
  in (origCont, newCont)

-- Function write the transformation changes in memory only
applyChanges' :: (GhcMonad m, SourceInfoTraversal node) => node dom SrcTemplateStage -> Target -> m ()
applyChanges' cmod target = do
          let newCont = stringToStringBuffer $ prettyPrint cmod
          utcTime <- liftIO $ getCurrentTime
          removeTarget $ targetId target 
          let target' = target {targetContents = Just (newCont, utcTime)}
          addTarget target'

-- function to write the changed code to the file
writeToFile :: FilePath -> FilePath -> String -> IO ()
writeToFile tdir file str = do 
  setCurrentDirectory tdir
  liftIO $ withBinaryFile file WriteMode $ \handle -> do
              hSetEncoding handle utf8
              hPutStr handle str
              hFlush handle
  return ()

getRawContentFromModule :: FilePath -> IO String
getRawContentFromModule fp = 
  withBinaryFile fp ReadMode $ \handle -> do
    hSetEncoding handle utf8
    StrictIO.hGetContents handle

replaceInFile :: Replacements -> FilePath -> IO ()
replaceInFile replacements filePath = do
  contents <- withBinaryFile filePath ReadMode $ \handle -> do
                hSetEncoding handle utf8
                StrictIO.hGetContents handle
  let modifiedContents = applyReplacements replacements contents
  withBinaryFile filePath WriteMode $ \handle -> do
              hSetEncoding handle utf8
              hPutStr handle modifiedContents
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

getCachedFieldsMap :: String -> String -> IO FieldsMap
getCachedFieldsMap workingDir gatewayName = do 
    filePath' <- lookupEnv "HT_GATEWAY_CACHE"
    let filePath = fromMaybe workingDir filePath'
    cachedFieldsMapStr <- withBinaryFile (filePath ++ "/CachedFields_" ++ gatewayName ++ ".txt") ReadWriteMode $ \handle -> do
                                                                            hSetEncoding handle utf8
                                                                            BS.hGetContents handle
    let (cachedFieldsMap :: FieldsMap) = case Aeson.decodeStrict (cachedFieldsMapStr) of
                                Just res -> res 
                                Nothing  -> trace ("Unable to decode") [] 
    return cachedFieldsMap           

-- Update the fieldsMap in the file after updating the values if already exist
updateCachedFieldsFile :: String -> String -> FieldsMap -> IO ()
updateCachedFieldsFile workingDir gatewayName fieldsMap = do 
  cachedFieldsMap <- getCachedFieldsMap workingDir gatewayName
  let updatedFieldsMap = updateFieldsMap cachedFieldsMap fieldsMap
  filePath' <- lookupEnv "HT_GATEWAY_CACHE"
  let filePath = fromMaybe workingDir filePath'
  withBinaryFile (filePath ++ "/CachedFields_" ++ gatewayName ++ ".txt") WriteMode $ \handle -> do
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


addGatewayModuleByPath :: FilePath -> ModuleName -> Ghc Target
addGatewayModuleByPath filePath moduleName
  = do 
       target <- guessTarget filePath Nothing
       addTarget target
       return target

addGatewayModule :: FilePath -> ModuleName -> Ghc Target
addGatewayModule filePath moduleName
  = do 
       setDirs [filePath]
       target <- guessTarget moduleName Nothing
       addTarget target
       return target

loadAllTargets :: Ghc Bool
loadAllTargets = do 
  res <- load LoadAllTargets
  pure $ case res of
    Succeeded -> True
    Failed    -> False

loadModSummaries :: Ghc ()
loadModSummaries = depanal [] False >> return ()

-- | Load the summary of a module given by the working directory and module name.
loadGatewayModule :: FilePath -> ModuleName -> Ghc (ModSummary, Target)
loadGatewayModule filePath moduleName
  = do 
       target <- guessTarget filePath Nothing
       addTarget target
       void $ load (LoadUpTo $ GHC.mkModuleName moduleName)
       targets <- getTargets
       liftIO $ print ("Target = " ++ (show' targets))
       summary <- getModSummary $ GHC.mkModuleName moduleName
       pure $ (summary, target)

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
  if shouldSet 
    then
      void $ setSessionDynFlags
        -- $ flip setOrUnset Opt_KeepHcFiles
        -- $ flip setOrUnset Opt_KeepHscppFiles
        -- $ flip setOrUnset Opt_KeepSFiles
        $ flip setOrUnset Opt_KeepTmpFiles
        $ flip setOrUnset Opt_KeepHiFiles
        $ flip setOrUnset Opt_KeepOFiles
        -- $ flip setOrUnset Opt_WriteInterface
        -- $ flip setOrUnset Opt_WriteHie
        $ dflags
    else pure ()

setIntermediateOutputDirs :: FilePath -> Ghc ()
setIntermediateOutputDirs path = do
  !flags <- getSessionDynFlags
  !_ <- setSessionDynFlags $ flags  {
                                            hiDir      = Just path,
                                            objectDir  = Just path,
                                            hieDir     = Just path,
                                            stubDir    = Just path,
                                            dumpDir    = Just path
                                        }
  pure ()


-- | Sets up basic flags and settings for GHC
initGhcFlagsForGateway' :: Bool -> Bool -> Ghc ()
initGhcFlagsForGateway' needsCodeGen errorsSuppressed = do
  dflags <- getSessionDynFlags
  let enabledExtensions = [
                    GHC.AllowAmbiguousTypes
                    ,GHC.BangPatterns
                    ,GHC.BlockArguments
                    ,GHC.ConstraintKinds
                    ,GHC.DataKinds
                    ,GHC.DeriveAnyClass
                    ,GHC.DeriveDataTypeable
                    ,GHC.DeriveFoldable
                    ,GHC.DeriveFunctor
                    ,GHC.DeriveGeneric
                    ,GHC.DeriveTraversable
                    ,GHC.DerivingStrategies
                    ,GHC.DerivingVia
                    ,GHC.DisambiguateRecordFields
                    ,GHC.DuplicateRecordFields
                    ,GHC.EmptyCase
                    ,GHC.EmptyDataDeriving
                    ,GHC.ExistentialQuantification
                    ,GHC.ExplicitForAll
                    ,GHC.ExplicitNamespaces
                    ,GHC.FlexibleContexts
                    ,GHC.FlexibleInstances
                    ,GHC.FunctionalDependencies
                    ,GHC.GADTs
                    ,GHC.GeneralizedNewtypeDeriving
                    ,GHC.ImplicitParams
                    ,GHC.InstanceSigs
                    ,GHC.KindSignatures
                    ,GHC.LambdaCase
                    ,GHC.MagicHash
                    ,GHC.MultiParamTypeClasses
                    ,GHC.MultiWayIf
                    ,GHC.RecordPuns
                    ,GHC.OverloadedLabels
                    ,GHC.OverloadedStrings
                    ,GHC.PackageImports
                    ,GHC.PartialTypeSignatures
                    ,GHC.PatternSynonyms
                    ,GHC.PolyKinds
                    ,GHC.QuasiQuotes
                    ,GHC.RankNTypes
                    ,GHC.RecordWildCards
                    ,GHC.ScopedTypeVariables
                    ,GHC.StandaloneDeriving
                    ,GHC.TemplateHaskell
                    ,GHC.TemplateHaskellQuotes
                    ,GHC.TupleSections
                    ,GHC.TypeApplications
                    ,GHC.TypeFamilies
                    ,GHC.TypeOperators
                    ,GHC.TypeSynonymInstances
                    ,GHC.UndecidableInstances
                    ,GHC.UnicodeSyntax
                    ,GHC.ViewPatterns
                  ]
      disabledExtensions = [
                            GHC.ImplicitPrelude
                          ]
      exposedPackages = [
                    "QuickCheck", "aeson", "amazonka", "amazonka-core", "amazonka-kms", "amazonka-ses", "async", "base", "base-compat", "base16", "base64", "base64-bytestring", "basement", "beam-core", "beam-large-records", "beam-mysql", "beam-postgres", "beam-sqlite", "binary", "byteable", "byteslice", "bytestring", "case-insensitive", "casing", "cereal", "cipher-aes", "constraints", "containers", "country", "cryptonite", "crypto-api", "cryptostore", "currency-codes", "data-default", "data-default-class", "deepseq", "digest", "directory", "dlist", "double-conversion", "email-validate", "errors", "euler-db", "euler-events-hs", "euler-hs", "euler-webservice",  "exceptions", "extra", "filepath", "fmt", "free", "generic-data", "generic-lens", "generic-random", "generics-sop", "ghc-hasfield-plugin", "ghc-prim", "hedis", "hex", "hopenssl", "HTTP", "http-api-data", "http-client", "http-client-tls", "http-types", "ieee", "inline-js", "inline-js-core", "iso8601-time", "jose", "jose-jwt", "jrec", "juspay-extra", "jwt", "large-anon", "large-generics", "large-records", "lens", "lens-aeson", "medea", "megaparsec", "memory", "mime-mail", "monad-parallel", "mtl", "named", "neat-interpolation", "network-uri", "newtype", "optics-core", "pcg-random", "pcre-heavy", "pcre-light", "pcre2", "pem", "postgresql-simple", "prettyprinter", "primitive", "quickcheck-instances", "random", "random-bytestring", "raven-haskell", "rawstring-qm", "record-dot-preprocessor", "record-hasfield", "recover-rtti", "reflection", "regex-compat", "regex-pcre", "regex-tdfa", "RSA", "safe", "safe-exceptions", "say", "scientific", "semigroupoids", "sequelize", "servant-client", "servant-server", "simple-cmd", "smtp-mail", "sort", "split", "stm", "string-conversions", "suspend", "tagsoup", "tasty", "tasty-discover", "tasty-hunit", "tasty-quickcheck", "template-haskell", "temporary", "text", "time", "timers", "timespan", "tinylog", "transformers", "ua-parser", "unix", "universum", "unordered-containers", "uri-encode", "url", "utf8-string", "uuid", "vector", "wai", "warp", "word8", "x509", "x509-store", "x509-validation", "xml-conduit", "zlib"
                ]
  dflags <- pure $ dflags { importPaths = []
                            --  , maxErrors = Nothing
                            , parMakeCount = Nothing
                            , warningFlags = EnumSet.empty
                            , hscTarget = if needsCodeGen then HscInterpreted else HscAsm -- HscNothing
                            , ghcLink = if needsCodeGen then LinkInMemory else NoLink
                            , packageDBFlags = [PackageDB (PkgConfFile "/nix/store/5gjrr6qww23j25wdyl1sm97s768myigf-ghc-8.8.4-with-packages/lib/ghc-8.8.4/package.conf.d"), PackageDB (PkgConfFile "/Users/piyush.garg/ht-gateway-task/euler-api-txns/dist-newstyle/packagedb/ghc-8.8.4")]
                            , ghcMode = CompManager
                            , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
                            --  , pluginModNames = pluginModNames dflags ++ [mkModuleName "Data.Record.Anon.Plugin", mkModuleName "RecordDotPreprocessor"]
                          }
  dflags <- pure $ foldr (\x r -> xopt_set r x) dflags enabledExtensions
  dflags <- pure $ foldr (\x r -> xopt_unset r x) dflags disabledExtensions
  dflags <- pure $ foldr (\x r -> r {packageFlags = ExposePackage x (PackageArg x) (ModRenaming True []) : packageFlags r}) dflags exposedPackages
  void $ setSessionDynFlags
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ flip gopt_set Opt_BreakOnError
    $ flip gopt_set Opt_BreakOnException
    $ flip gopt_set Opt_HideAllPackages -- hide all packages
    $ (if errorsSuppressed then
                                    flip gopt_set Opt_DeferTypedHoles
                                  . flip gopt_set Opt_DeferTypeErrors
                                  . flip gopt_set Opt_DeferOutOfScopeVariables
                           else id)
    $ dflags

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

isDistinguishable :: String -> DiffMap -> Bool 
isDistinguishable typ diffMap = fromMaybe True $ Map.lookup typ diffMap

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
        getFieldsFromFieldDecl _ = []
getFieldsFromDecl _ = []

getRecordTypes :: HT.Module -> [String]
getRecordTypes source = 
  let (decls :: [Decl]) = source ^? biplateRef
      (recordDecls :: [Decl]) = filter isRecordType decls
  in getDeclName <$> recordDecls

isRecordType :: Decl -> Bool 
isRecordType (DataDecl _ _ (NameDeclHead _) (AnnList [RecordConDecl _ _]) _) = True 
isRecordType _ = False

setDirs :: [FilePath] -> Ghc ()
setDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = workingDirs }


-- ================== Refactoring ===================

gw :: String -> String -> ParsedSource -> Replacements
gw typ fld source = 
  let (dataDecls' :: [TyClDecl GhcPs]) = source ^? biplateRef
      ([dataDecl] :: [TyClDecl GhcPs]) = filter (dataDeclFromTypeName typ) dataDecls'
      replacement = getFieldReplacement dataDecl fld
  in [replacement]
  where    
    dataDeclFromTypeName :: String -> TyClDecl GhcPs -> Bool
    dataDeclFromTypeName typName (GHC.DataDecl _ (L _ name) _ _ _) = (show' name == typName)
    dataDeclFromTypeName _ _ = False

    getFieldReplacement :: TyClDecl GhcPs -> String -> Replacement 
    getFieldReplacement decl fld = 
      let [(ConDeclH98 {con_args = GHC.RecCon (unLoc -> fields)})] = fmap unLoc $ dd_cons $ tcdDataDefn decl
          maybeField = find (\x -> fld `elem` (fmap (show' . unLoc) $ cd_fld_names x)) (fmap unLoc fields)
      in
        maybe mkEmptyReplacement getTypeReplacementFromField maybeField

    getTypeReplacementFromField :: GHC.ConDeclField GhcPs -> Replacement
    getTypeReplacementFromField (ConDeclField _ _ typ _) = 
      let typSpan = mkSpanFromLocated typ
      in
        maybe mkEmptyReplacement (\x -> mkAppend (mkSpan (getStartLine x, getStartCol x) (getStartLine x, getStartCol x)) "Maybe ") typSpan

