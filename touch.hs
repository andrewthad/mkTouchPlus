import Data.Char (toLower, toUpper, isSpace, isSeparator, isPunctuation)
import Data.Function (on)
import Data.List (intercalate, intersperse, groupBy, scanl)
import System.IO (writeFile)

import System.Directory -- (createDirectory)
import System.Environment
import System.IO
import System.IO.Error

-- TODO: default format for extension: remove spaces + all lowercase. apply this separately from name format. Use (filename, ext) tuple again? Probably not
-- TODO: coloured output
-- TODO: user prompt to remove illegal characters from file name or otherwise skip operation
-- TODO: use where to simplify all eitherEq to eitherSep (for each of these three helper functions)
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

-- extFormat :: [String] -> [String]
-- extFormat = mapLast concat

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

sepChoice :: String -> ([String] -> [String])
sepChoice sep | eitherSep "h" "hyphenSep" = hyphenSep
              | eitherSep "s" "snakeSep"  = snakeSep
              | eitherSep "d" "dotSep"    = dotSep
              | eitherSep "S" "spaceSep"  = spaceSep
              | eitherSep "n" "noSep"     = id
              | otherwise                 = id
                where eitherSep = eitherEq sep

caseChoice :: String -> ([[String]] -> [[String]])
caseChoice charCase | eitherEq charCase "n" "noCase"    = mapButLast $ id
                    | eitherEq charCase "l" "lowerCase" = mapButLast $ lowerCase
                    | eitherEq charCase "u" "upperCase" = mapButLast $ upperCase
                    | eitherEq charCase "t" "titleCase" = mapButLast $ titleCase
                    | eitherEq charCase "c" "camelCase" = mapButLast $ camelCase
                    | otherwise                  = id

createChoice :: String -> (String -> IO ())
createChoice createOp | eitherEq createOp "t" "touch" = create "File" doesFileExist createFile
                      | eitherEq createOp "m" "mkdir" = create "Folder" doesDirectoryExist createDirectory
                      | otherwise                     = putId

extFormat = mapLast (putToList . concat)

maker createOp sep charCase name = createChoice createOp $ concat $ dot $ (mapButLast $ sepChoice sep) $ extFormat $ caseChoice charCase $ tokens name

t = maker "touch" "" ""
m = maker "mkdir" "" ""
o = maker "other" "h" "u"
