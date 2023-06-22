module Language.Haskell.Tools.Debug.LegacyCode where 

-- -- Create and recur over the batches generated
--     (fieldsMap :: FieldsMap) <- concat <$> forM (makeBatches 5 recResTypes'') (\recResTypes -> do
--       -- -- Copy the files in a synchronized manner
--       -- forM recResTypes (\(typ, fld) -> do
--       --   let !modDir  = workingDir ++ "temp_" ++ typ ++ "_" ++ fld ++ "/"
--       --       !modFile = modDir ++ "Types.hs"

--       --   -- create temp directory
--       --   liftIO $ createDirectoryIfMissing False $ modDir
--       --   -- Copy Files
--       --   liftIO $ copyFile (workingDir ++ "Types.hs") modFile
--       --   liftIO $ copyFile (workingDir ++ "Transforms.hs") (modDir ++ "Transforms.hs")
--       --   liftIO $ copyFile (workingDir ++ "Flow.hs") (modDir ++ "Flow.hs")
--       --  )

--       -- liftIO $ putStrLn "Done with copying the files in batch"

--       -- Process the batch asynchronously in multi-threaded environment
--       !usedFields' <- mapConcurrently (\(typ, fld) -> runGhc (Just libdir) $ do
--           -- Logic for caching the intermediate results and picking from cache if already done
--           let (!isCached, !cacheRes) = isCachedField (typ, fld) cachedFieldsMap
--           !_ <- liftIO $ putStrLn $ "Check in cache"
--           if isCached 
--           then trace ("Found in Cache " ++ show (typ, fld, cacheRes)) $ return [(typ, fld, cacheRes)]
--           else do 
--             !_ <- liftIO $ putStrLn $ "Not found in cache"
--             let !command = "gw " ++ typ ++ " " ++ fld
--                 !modDir  = workingDir ++ "temp_" ++ typ ++ "_" ++ fld ++ "/"
--                 !modFile = modDir ++ "Types.hs"
--             !_ <- liftIO $ putStrLn $ "Command = " ++ command
          
--             -- create temp directory
--             liftIO $ createDirectoryIfMissing False $ modDir
--             -- Copy Files
--             liftIO $ copyFile (workingDir ++ "Types.hs") modFile
--             liftIO $ copyFile (workingDir ++ "Transforms.hs") (modDir ++ "Transforms.hs")
--             liftIO $ copyFile (workingDir ++ "Flow.hs") (modDir ++ "Flow.hs")
--             -- Copy finish log
--             liftIO $ putStrLn "Done with copying the file"

--             -- setSession hsc_env
--             -- setSessionDynFlags hsc_env_flags
--             initGhcFlagsForGateway' True False
--             !_ <- useDirs [modDir]
--             !_ <- useFlags args

--             !oldFlags <- getSessionDynFlags
--             !_ <- setSessionDynFlags $ setTmpDir modDir oldFlags

--             setTargets []
--             addGatewayModule modFile moduleName
--             addGatewayModule (modDir ++ "Transforms.hs") "Gateway.Paytm.Transforms"
--             addGatewayModule (modDir ++ "Flow.hs") "Gateway.Paytm.Flow"
          
--             !transformed <- performCommand builtinRefactorings (splitOn " " command)
--                                           (Right ((SourceFileKey (moduleSourceFile moduleName) moduleName), sourced))
--                                           []

--             case transformed of
--               Right changes -> do
--                 !curr <- forM changes $ \case
--                   ContentChanged (mod, correctlyTransformed) -> do
--                     liftIO $ putStrLn $ "=========== transformed AST (" ++ (mod ^. sfkModuleName) ++ "):"
--                     -- liftIO $ putStrLn $ srcInfoDebug correctlyTransformed
--                     liftIO $ putStrLn $ "=========== transformed & prettyprinted (" ++ (mod ^. sfkModuleName) ++ "):"
--                     let prettyPrinted = prettyPrint correctlyTransformed
--                     -- liftIO $ putStrLn prettyPrinted
--                     liftIO $ putStrLn $ "=========== Write into file (" ++ (mod ^. sfkModuleName) ++ "):"
--                     -- (undo, unifiedDiff, origCont) <- liftIO $ applyChanges correctlyTransformed mod workingDir modFile
--                     -- clearModules [ms]
--                     -- applyChanges' sourced typeTarget
--                     let (origCont, newCont) = getContentsFromModules correctlyTransformed sourced
--                     liftIO $ writeToFile modDir modFile newCont
--                     loadAllTargets
--                     -- loadModSummaries
--                     liftIO $ putStrLn $ "=========== Validate Changes :"
--                     -- result <- liftIO $ runGhc (Just libdir) $ do
--                     --     -- Setup flags
--                     --     initGhcFlagsForGateway
--                     --     _ <- useDirs [workingDir]
--                     --     _ <- useFlags args
--                     --     setTargets []
--                     --     mso <- loadGatewayModule modFile moduleName
--                     --     res <- validateChanges ["Gateway.Paytm.Transforms", "Gateway.Paytm.Flow"] (fmap (workingDir ++ ) ["Transforms.hs", "Flow.hs"])
--                     --     liftIO $ putStrLn $ "Typecheck result - " ++ show res
--                     --     return res
--                     res <- validateChanges ["Gateway.Paytm.Transforms", "Gateway.Paytm.Flow"] (fmap (modDir ++ ) ["Transforms.hs", "Flow.hs"])
--                     liftIO $ putStrLn $ "Typecheck result - " ++ show res
--                     result <- return res
--                     liftIO $ putStrLn "==========="
--                     -- liftIO $ writeToFile workingDir modFile origCont
--                     if result then return (typ, fld, False) else return (typ, fld, True)
--                   ModuleRemoved mod -> do
--                     liftIO $ putStrLn $ "=========== module removed: " ++ mod
--                     return (typ, fld, True)
--                   ModuleCreated mod cont _ -> do
--                     liftIO $ putStrLn $ "=========== created AST (" ++ mod ++ "):"
--                     -- liftIO $ putStrLn $ srcInfoDebug cont
--                     liftIO $ putStrLn $ "=========== created & prettyprinted (" ++ mod ++ "):"
--                     -- let prettyPrinted = prettyPrint cont
--                     -- liftIO $ putStrLn prettyPrinted
--                     return (typ, fld, True) 
--                 -- fmap (curr ++ ) r
--                 liftIO $ removeDirectoryRecursive $ modDir
--                 return curr
--               Left transformProblem -> do
--                 liftIO $ putStrLn "==========="
--                 liftIO $ putStrLn transformProblem
--                 liftIO $ putStrLn "==========="
--                 liftIO $ removeDirectoryRecursive $ modDir
--                 return [(typ, fld, True)]
--                 -- r                  
--         ) recResTypes

--       let !(usedFields :: FieldsMap) = concat usedFields' 

--       -- Perform cleanup of temp directory and files
--       -- forM recResTypes (\(typ, fld) -> do
--       --   let !modDir  = workingDir ++ "temp_" ++ typ ++ "_" ++ fld ++ "/"
          
--       --   -- create temp directory
--       --   liftIO $ removeDirectoryRecursive $ modDir
--       --  )
      
--       -- Print the used Fields
--       liftIO $ putStrLn $ "Used Fields in current batch = " ++ show usedFields

--       -- Update the cached fields result
--       liftIO $ updateCachedFieldsFile gatewayName usedFields

--       return usedFields
--      )