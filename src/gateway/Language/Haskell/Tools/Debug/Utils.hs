{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Tools.Debug.Utils where 

import Outputable
import DynFlags
import qualified HsDumpAst as GHC
import Data.Data
import Data.List
import SrcLoc

fst3 :: (a, b, c) -> a 
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

show' :: Outputable a => a -> String 
show' = showSDocUnsafe . ppr

showF :: Outputable a => DynFlags -> a -> String
showF flags = showSDoc flags . ppr 

showAst :: Data a => a -> String 
showAst = showSDocUnsafe . GHC.showAstData GHC.NoBlankSrcSpan 

replaceDotWithSlash :: String -> String
replaceDotWithSlash s = foldr (\x r -> if x == '.' then '/':r else x:r) "" s  

makeBatches :: Int -> [a] -> [[a]]
makeBatches lim ls 
  | length ls <= lim = [ls]
  | otherwise        = take lim ls : makeBatches lim (drop lim ls)

printLog :: String -> (String, String) -> IO ()
printLog heading typ = putStrLn $ "INFO_LOG :: " ++ heading ++ " ==> " ++ show typ

replaceInContent :: String -> String -> Span -> String 
replaceInContent origContent replacedWord span = 
  unlines $ fst $ foldr (checkAndReplaceInLine replacedWord span) ([], False) $ zip [1..] (lines origContent)
  where 
    checkAndReplaceInLine :: String -> Span -> (Int, String) -> ([String], Bool) -> ([String], Bool)
    checkAndReplaceInLine replacedWord span (lineNumber, line) (res, isReplaced) = 
      let res' = foldr (replaceCharIfInRange replacedWord span lineNumber) ("", isReplaced) $ zip [1..] line
      in
        (fst res' : res, snd res' || isReplaced)

    replaceCharIfInRange :: String -> Span -> Int -> (Int, Char) -> (String, Bool) -> (String, Bool)
    replaceCharIfInRange replacedWord span lineNumber (colNumber, char) (res, isReplaced) =
      if isInsideSpan (lineNumber, colNumber) span && isReplaced == False 
        then (replacedWord ++ res, True)
      else if isInsideSpan (lineNumber, colNumber) span
        then (res, isReplaced)
      else 
        (char : res, isReplaced)


-- ================ FILE MODIFICATION UTILS ================

type Location = (Int, Int)
type Span = (Location, Location)
type Replacement = (Span, Bool, Bool, String) -- Span, isDone, isAppend, replace/append string
type Replacements = [Replacement]
type LocEntry = (Char, Location, Bool)
type LocMap = [LocEntry] -- character, originalLocation, isIncluded

mkLocation :: Int -> Int -> Location
mkLocation lineNo columnNo = (lineNo, columnNo)

mkSpan :: Location -> Location -> Span
mkSpan loc1 loc2 = (loc1, loc2)

mkReplacement :: Span -> String -> Replacement
mkReplacement sp replaceWord = (sp, False, False, replaceWord)

mkAppend :: Span -> String -> Replacement
mkAppend sp appendWord = (sp, False, True, appendWord)

mkEmptyReplacement :: Replacement
mkEmptyReplacement = mkReplacement (mkSpan (mkLocation 0 0) (mkLocation 0 0)) ""

mkSpanFromLocated :: Located a -> Maybe Span 
mkSpanFromLocated (L (RealSrcSpan loc) _) = 
  let startLine = srcSpanStartLine loc 
      endLine   = srcSpanEndLine loc 
      startCol  = srcSpanStartCol loc 
      endCol    = srcSpanEndCol loc
  in
    Just $ mkSpan (mkLocation startLine startCol)  (mkLocation endLine endCol)
mkSpanFromLocated _ = Nothing

getStartLine :: Span -> Int
getStartLine = fst . fst

getStartCol :: Span -> Int
getStartCol = snd . fst

getEndLine :: Span -> Int
getEndLine = fst . snd

getEndCol :: Span -> Int
getEndCol = snd . snd

isInsideSpan :: Location -> Span -> Bool
isInsideSpan loc (sLoc, eLoc) = 
  if fst loc <= 0 || snd loc <= 0
    then False
    else (sLoc <= loc) && (loc <= eLoc)

getNoColumnLocMap :: String -> Int -> LocMap
getNoColumnLocMap line lineNo = fmap (\ch -> (ch, mkLocation lineNo (-1), True)) line

lineToLocMap :: (String, Int) -> LocMap
lineToLocMap (line, lineNo) = fmap (\(x, columnNo) -> (x, mkLocation lineNo columnNo, True)) $ zip line [1..]

-- creates LocMap from the file
-- first convert into lines and zip them
-- then traverse over the line for the column number
createLocMap :: String -> LocMap
createLocMap file = 
  let content = lines file
  in concatMap lineToLocMap $ zip content [1..]

-- Converts LocMap back to String
-- Check if it's the first line or the character is in the same line
-- If yes, then just append to result
-- Else, first add '\n' and then add character to res
locMapToString :: LocMap -> String
locMapToString mp = fst $ foldr checkAndAdd ("", getMaxLineNumber mp 0) mp
  where
    checkAndAdd :: LocEntry -> (String, Int) -> (String, Int)
    checkAndAdd (ch, (lineNo, _), isIncluded) (res, prevLine) =
      if isIncluded
        then
          if lineNo == prevLine
            then (ch:res, lineNo)
            else ((ch : replicate (prevLine - lineNo) '\n') ++ res, lineNo)
        else (res, prevLine)
        
    getMaxLineNumber :: LocMap -> Int -> Int
    getMaxLineNumber mp def = foldr (\(_, (lineNo, _), _) res -> max lineNo res) def mp
    
applyReplacement :: LocMap -> Replacement -> LocMap
applyReplacement mp rep = checkAndUpdate rep mp
  where
    checkAndUpdate :: Replacement -> LocMap -> LocMap
    checkAndUpdate _ [] = []
    checkAndUpdate rep@(sp, isDone, isAppend, replaceWord) (locEntry@(ch, loc, isIncluded) : locEntries) =
      if isInsideSpan loc sp
        then
          if isDone
            then 
              let modifiedLocEntry = (ch, loc, isAppend && isIncluded)
              in modifiedLocEntry : checkAndUpdate rep locEntries
            else 
              let newRep = (sp, True, isAppend, replaceWord) 
                  modifiedLocEntry = (ch, loc, isAppend && isIncluded)
                  addedWordLocMap = getNoColumnLocMap replaceWord $ fst loc
              in addedWordLocMap ++ (modifiedLocEntry : checkAndUpdate newRep locEntries)
        else locEntry : checkAndUpdate rep locEntries

applyReplacements :: Replacements -> String -> String
applyReplacements replacements file = locMapToString $ foldl' applyReplacement (createLocMap file) replacements

newline :: IO ()
newline = putStrLn ""