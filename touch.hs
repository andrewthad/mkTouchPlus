import Data.Char (toLower, toUpper, isSpace, isSeparator, isPunctuation)
import Data.Function (on)
import Data.List (intercalate, intersperse, groupBy, scanl)
import System.IO (writeFile)

import System.Directory -- (createDirectory)
import System.Environment
import System.IO
import System.IO.Error

-- TODO: coloured output
-- TODO: user prompt to remove illegal characters from file name or otherwise skip operation
-- TODO: 'smart' is an alternative to touch and mkdir that calls either of those two for each file even if multiple are given. it calls mkdir if no file extension and touch if file extension exists
-- TODO: make extFormat use the same options as the name formatting, but with a different default value

-- Utilities ----------

isSep :: Char -> Bool
isSep c | isSeparator c = True | isPunctuation c = True | otherwise = False

eitherEq :: (Eq a) => a -> a -> a -> Bool
eitherEq a = (||) `on` (a ==)

groupStr :: Char -> String -> [String]
groupStr c s = let (start, end) = break (== c) s
                in start : if null end then [] else groupStr c (tail end)

groupSep :: String -> [String]
groupSep = groupBy ((==) `on` isSep)

mapLast, mapButLast, mapButFirst :: (a -> a) -> [a] -> [a]

mapLast f [] = []
mapLast f [x] = [f x]
mapLast f (x:xs) = x : mapLast f xs

mapButLast f [] = []
mapButLast f [x] = [x]
mapButLast f (x:xs) = f x : mapButLast f xs

mapButFirst f [] = []
mapButFirst f [x] = [x]
mapButFirst f (x:xs) = x : map f xs

putId, createFile :: String -> IO ()

putId = putStrLn . id
createFile s = writeFile s ""

putToList :: a -> [a]
putToList x = [x]

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

dot :: [[String]] -> [String]
dot = foldr (\x y -> if y == [] then x ++ y else x ++ ["."] ++ y) []

-- Cases ----------

title :: String -> String
title (c:cs) = toUpper c : cs

lowerCase, upperCase, titleCase, camelCase :: [String] -> [String]

lowerCase = map $ map toLower
upperCase = map $ map toUpper
titleCase = map title
camelCase = mapButFirst title

-- Extension formatting ----------

extFormat :: [[a]] -> [[a]]
extFormat = putToList . concat

-- IO ----------

msg :: String -> String -> String -> IO ()
msg op form s = putId $ take 14 (op ++ " " ++ form) ++ ": " ++ s

createMsg, skipMsg :: String -> String -> IO ()

createMsg op s = msg op "Created" s
skipMsg op s = msg op "Skipped" s

create :: String -> (String -> IO Bool) -> (String -> IO ()) -> String -> IO ()
create form existF makeF s = if s == ""
                                then skipMsg form ("No " ++ form ++ " name provided.")
                                else do exists <- existF s
                                        if exists
                                           then skipMsg form ("The " ++ form ++ " '" ++ s ++ "' already exists, but it hasn't been overwritten by this operation")
                                           else do
                                               makeF s
                                               createMsg form ("'" ++ s ++ "'")

-- Composition ----------

sepChoice, caseChoice :: String -> ([String] -> [String])

sepChoice sep | eitherSep "h" "hyphenSep" = hyphenSep
              | eitherSep "s" "snakeSep"  = snakeSep
              | eitherSep "d" "dotSep"    = dotSep
              | eitherSep "S" "spaceSep"  = spaceSep
              | eitherSep "n" "noSep"     = id
              | otherwise                 = id
                where eitherSep = eitherEq sep

caseChoice charCase | eitherCase "n" "noCase"    = id
                    | eitherCase "l" "lowerCase" = lowerCase
                    | eitherCase "u" "upperCase" = upperCase
                    | eitherCase "t" "titleCase" = titleCase
                    | eitherCase "c" "camelCase" = camelCase
                    | otherwise                  = id
                      where eitherCase = eitherEq charCase

extChoice ext | null ext                  = extFormat
              | eitherExt "e" "extFormat" = extFormat
              | otherwise                 = sepChoice ext
                where eitherExt = eitherEq ext

createChoice :: String -> (String -> IO ())
createChoice createOp | eitherCreate "t" "touch" = create "File" doesFileExist createFile
                      | eitherCreate "m" "mkdir" = create "Folder" doesDirectoryExist createDirectory
                      | otherwise                = putId -- TODO: change to error message later
                        where eitherCreate = eitherEq createOp

maker createOp sep charCase ext name = createChoice createOp $ concat $ dot $ (mapButLast $ sepChoice sep) $ (mapLast $ extChoice ext) $ (mapButLast $ caseChoice charCase) $ tokens name

t = maker "touch" "" "" ""
m = maker "mkdir" "" "" ""
o = maker "other" "h" "u" ""
