import Data.Char (toLower, toUpper, isSpace, isSeparator, isPunctuation)
import Data.Function (on)
import Data.List (intercalate, intersperse, groupBy, scanl)
import System.IO (appendFile)

import System.Directory -- (createDirectory)
import System.Environment
import System.IO
import System.IO.Error

-- Utilities ----------

isSep :: Char -> Bool
isSep c | isSeparator c = True | isPunctuation c = True | otherwise = False

eitherEq :: (Eq a) => a -> a -> a -> Bool
eitherEq a = (||) `on` (a ==)

groupStr :: Char -> String -> [String]
groupStr c = groupBy ((==) `on` (== c))

groupSep :: String -> [String]
groupSep = groupBy ((==) `on` isSep)

mapButLast :: (a -> a) -> [a] -> [a]
mapButLast f [] = []
mapButLast f [x] = [x]
mapButLast f (x:xs) = f x : mapButLast f xs

mapButFirst :: (a -> a) -> [a] -> [a]
mapButFirst f [] = []
mapButFirst f [x] = [x]
mapButFirst f (x:xs) = x : map f xs

-- Tokenisation ----------

notNull :: [[a]] -> [[a]]
notNull = filter (not . null)

sections :: String -> [String]
sections = notNull . groupStr '.'

tokens :: String -> [[String]]
tokens s = words <$> sections s

-- Separators ----------

hyphenSep, snakeSep, dotSep :: [String] -> [String]

hyphenSep = intersperse "-"
snakeSep = intersperse "_"
dotSep = intersperse "."
spaceSep = intersperse " "

-- Cases ----------

title :: String -> String
title (c:cs) = toUpper c : cs

lowerCase, upperCase, titleCase, camelCase :: [[String]] -> [[String]]

lowerCase = mapButLast $ map $ map toLower
upperCase = mapButLast $ map $ map toUpper
titleCase = mapButLast $ map title
camelCase = mapButLast $ mapButFirst title

-- IO ----------

touch, mkDir :: String -> IO ()

touch s = do
    exists <- doesFileExist s
    if exists
       then putStrLn $ show $ "The file '" ++ s ++ "' already exists, but it hasn't been overwritten by this operation"
       else do
           appendFile s ""
           putStrLn $ show $ "Created file: '" ++ s ++ "'"

mkDir = do
    exists <- doesDirectoryExist s
    if exists
       then putStrLn $ show $ "The directory '" ++ s ++ "' already exists, but it hasn't been overwritten by this operation"
       else do
           createDirectory s
           putStrLn $ show $ "Created directory: '" ++ s ++ "'"


-- Composition ----------

maker io sep charCase name = io
                           $ concat
                           $ sepChoice sep
                           $ concat
                           $ caseChoice charCase
                           $ tokens name
    where eitherSep = eitherEq sep
          eitherCase = eitherEq charCase
          sepChoice sep | eitherSep "h" "hyphenSep" = hyphenSep
                        | eitherSep "s" "snakeSep"  = snakeSep
                        | eitherSep "d" "dotSep"    = dotSep
                        | eitherSep "S" "spaceSep"  = spaceSep
                        | eitherSep "n" "noSep"     = id
                        | otherwise                 = id
          caseChoice charCase | eitherCase "n" "noCase"    = id
                              | eitherCase "l" "lowerCase" = lowerCase
                              | eitherCase "u" "upperCase" = upperCase
                              | eitherCase "t" "titleCase" = titleCase
                              | eitherCase "c" "camelCase" = camelCase
                              | otherwise                  = id
          io | eitherCase "t" "touch" = touch
             | eitherCase "m" "mkdir" = mkDir
             | otherwise              = putStrLn . show

t = maker "touch" "" ""
