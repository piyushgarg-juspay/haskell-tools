{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Debug.Utils where 

import Outputable


fst3 :: (a, b, c) -> a 
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

show' :: Outputable a => a -> String 
show' = showSDocUnsafe . ppr

replaceDotWithSlash :: String -> String
replaceDotWithSlash s = foldr (\x r -> if x == '.' then '/':r else x:r) "" s  

makeBatches :: Int -> [a] -> [[a]]
makeBatches lim ls 
  | length ls <= lim = [ls]
  | otherwise        = take lim ls : makeBatches lim (drop lim ls)

printLog :: String -> (String, String) -> IO ()
printLog heading typ = putStrLn $ "INFO_LOG :: " ++ heading ++ " ==> " ++ show typ