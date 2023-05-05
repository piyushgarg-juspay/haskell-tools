 {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Haskell.Tools.Debug where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Control.Reference ((^.))
import Data.List.Split (splitOn)
import Data.Maybe (Maybe(..), fromJust)
import GHC.Generics (Generic(..))
import System.FilePath (pathSeparator, (</>), (<.>))

import GHC hiding (loadModule)
import GHC.Paths ( libdir )
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import StringBuffer (hGetStringBuffer)
import Outputable
import HscTypes
import TcRnDriver
import TcRnTypes
import TcRnMonad
import Data.IORef
import DynFlags
import Avail

import Language.Haskell.Tools.AST (NodeInfo(..))
import Language.Haskell.Tools.BackendGHC
import Language.Haskell.Tools.Debug.DebugGhcAST ()
import Language.Haskell.Tools.Debug.RangeDebug (srcInfoDebug)
import Language.Haskell.Tools.Debug.RangeDebugInstances ()
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import qualified Language.Haskell.GHC.ExactPrint as EP
import Data.Either

import System.IO.Strict as StrictIO (hGetContents)
import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import Data.Algorithm.DiffContext (prettyContextDiff, getContextDiff)
import System.IO
import System.Directory
import Text.PrettyPrint as PP (text, render)


replaceDotWithSlash :: String -> String
replaceDotWithSlash s = foldr (\x r -> if x == '.' then '/':r else x:r) "" s  

-- | Should be only used for testing
demoRefactor :: String -> String -> [String] -> String -> IO ()
demoRefactor command workingDir args moduleName =
  runGhc (Just libdir) $ do
    initGhcFlags
    _ <- useFlags args
    useDirs [workingDir]

    liftIO $ putStrLn "=========== parsed source:"
    ms <- loadModule workingDir moduleName

    p <- parseModule ms
    let annots = pm_annotations $ p
    mp <- liftIO $ EP.parseModule (workingDir ++ replaceDotWithSlash moduleName ++ ".hs")
    let mp' = rights [mp]
    liftIO $ putStrLn $ foldr (\x r -> EP.exactPrint (snd x) (fst x) ++ r) "" mp'
    liftIO $ putStrLn "=========== tokens:"
    -- liftIO $ putStrLn $ show (fst annots)
    liftIO $ putStrLn "=========== comments:"
    -- liftIO $ putStrLn $ show (snd annots)
    liftIO $ putStrLn "=========== renamed source:"

    (rnSrc, tcSrc) <- ((\t -> (tm_renamed_source t, typecheckedSource t)) <$> typecheckModule p)
                         `gcatch` \(e :: SomeException) -> forcedTypecheck ms p
    -- liftIO $ putStrLn $ show rnSrc

    -- liftIO $ putStrLn $ show (fromJust $ tm_renamed_source t)
    liftIO $ putStrLn "=========== typechecked source:"
    -- liftIO $ putStrLn $ show tcSrc

    let hasCPP = Cpp `xopt` ms_hspp_opts ms

    liftIO $ putStrLn "=========== parsed:"
    --transformed <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule (pm_parsed_source p)
    parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms (pm_parsed_source p)
    liftIO $ putStrLn $ srcInfoDebug parseTrf

    liftIO $ putStrLn "=========== typed:"
    transformed <- addTypeInfos tcSrc =<< (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModuleRename ms parseTrf (fromJust $ rnSrc) (pm_parsed_source p))
    liftIO $ putStrLn $ srcInfoDebug transformed

    liftIO $ putStrLn "=========== ranges fixed:"
    sourceOrigin <- if hasCPP then liftIO $ hGetStringBuffer (workingDir </> map (\case '.' -> pathSeparator; c -> c) moduleName <.> "hs")
                              else return (fromJust $ ms_hspp_buf $ pm_mod_summary p)
    let commented = fixRanges $ placeComments (fst annots) (getNormalComments $ snd annots) $ fixMainRange sourceOrigin transformed
    liftIO $ putStrLn $ srcInfoDebug commented

    liftIO $ putStrLn "=========== cut up:"
    let cutUp = cutUpRanges commented
    liftIO $ putStrLn $ srcInfoDebug cutUp
    liftIO $ putStrLn $ show $ getLocIndices cutUp
    liftIO $ putStrLn $ show $ mapLocIndices sourceOrigin (getLocIndices cutUp)

    liftIO $ putStrLn "=========== sourced:"
    let sourced = (if hasCPP then extractStayingElems else id) $ rangeToSource sourceOrigin cutUp
    liftIO $ putStrLn $ srcInfoDebug sourced
    liftIO $ putStrLn "=========== pretty printed:"
    let prettyPrinted = prettyPrint sourced
    liftIO $ putStrLn prettyPrinted
    transformed <- performCommand builtinRefactorings (splitOn " " command)
                                  (Right ((SourceFileKey (moduleSourceFile moduleName) moduleName), sourced))
                                  []
    case transformed of
      Right changes -> do
        forM_ changes $ \case
          ContentChanged (mod, correctlyTransformed) -> do
            liftIO $ putStrLn $ "=========== transformed AST (" ++ (mod ^. sfkModuleName) ++ "):"
            liftIO $ putStrLn $ srcInfoDebug correctlyTransformed
            liftIO $ putStrLn $ "=========== transformed & prettyprinted (" ++ (mod ^. sfkModuleName) ++ "):"
            let prettyPrinted = prettyPrint correctlyTransformed
            liftIO $ putStrLn prettyPrinted
            liftIO $ putStrLn $ "=========== Write into file (" ++ (mod ^. sfkModuleName) ++ "):"
            liftIO $ applyChanges correctlyTransformed mod workingDir
            liftIO $ putStrLn "==========="
          ModuleRemoved mod -> do
            liftIO $ putStrLn $ "=========== module removed: " ++ mod
          ModuleCreated mod cont _ -> do
            liftIO $ putStrLn $ "=========== created AST (" ++ mod ++ "):"
            liftIO $ putStrLn $ srcInfoDebug cont
            liftIO $ putStrLn $ "=========== created & prettyprinted (" ++ mod ++ "):"
            let prettyPrinted = prettyPrint cont
            liftIO $ putStrLn prettyPrinted
      Left transformProblem -> do
        liftIO $ putStrLn "==========="
        liftIO $ putStrLn transformProblem
        liftIO $ putStrLn "==========="

-- Function Apply Changes
applyChanges cmod mod tdir = do
          let m = cmod 
              n = mod
              diffMode = False
          setCurrentDirectory tdir
          let newCont = prettyPrint m
              file = n ^. sfkFileName
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
