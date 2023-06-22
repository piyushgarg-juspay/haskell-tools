{-# LANGUAGE BangPatterns #-}

module Main where

import Language.Haskell.Tools.Debug (demoRefactor)
import System.Environment (getProgName, getArgs)

import Debug.Trace (trace)

main :: IO ()
main = do args <- getArgs
          !_ <- trace ("Args = " ++ show args) $ return ()
          if length args < 3 then do progName <- getProgName
                                     putStrLn (progName ++ " <command/-> <workingdir> <gatewayname> [arguments]")
                             else demoRefactor (dropWhile (=='-') (args !! 0)) (args !! 1) (drop 3 args) (args !! 2)