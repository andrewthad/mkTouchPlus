import Data.Char (toLower, toUpper, isSpace, isSeparator, isPunctuation)
import Data.Function (on)
import Data.List (intercalate, intersperse, groupBy, scanl)
import System.IO (writeFile)

import System.Directory -- (createDirectory)
import System.Environment
import System.IO
import System.IO.Error

-- TODO: default format for extension: remove spaces + all lowercase. apply this separately from name format. Use (filename, ext) tuple again? Probably not
-- TODO: make isSep more succinct
-- TODO: revert back to the old groupStr that strips out punctuation?
-- TODO: add error message for no file name provided
-- TODO: coloured output
-- TODO: column formatting:
-- Created File:
-- Skipped Directory: <-- longest column. so width is based on this
-- (titlecase the column)
-- titlecased, coloured column to its own function
-- TODO: user prompt to remove illegal characters from file name or otherwise skip operation
-- TODO: use where to simplify all eitherEq to eitherSep (for each of these three helper functions)
-- TODO: 'smart' is an alternative to touch and mkdir that calls either of those two for each file even if multiple are given. it calls mkdir if no file extension and touch if file extension exists

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

putShow, createFile :: String -> IO ()

putShow = putStrLn . show
createFile s = writeFile s ""

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

col :: String -> String -> String
col op s = take 14 (op ++ " " ++ s) ++ ": "

createCol, skipCol :: String -> String

createCol = col "Created"
skipCol = col "Skipped"

create :: String -> (String -> IO Bool) -> (String -> IO ()) -> String -> IO ()
create form existF makeF s = do
    if s == ""
       then putShow $ skipCol form ++ "No " ++ form ++ " name provided."
       else do exists <- existF s
               if exists
                  then putShow $ "The " ++ form ++ " '" ++ s ++ "' already exists, but it hasn't been overwritten by this operation"
                  else do
                      makeF s
                      putShow $ createCol form ++ "'" ++ s ++ "'"

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
caseChoice charCase | eitherEq charCase "n" "noCase"    = id
                    | eitherEq charCase "l" "lowerCase" = lowerCase
                    | eitherEq charCase "u" "upperCase" = upperCase
                    | eitherEq charCase "t" "titleCase" = titleCase
                    | eitherEq charCase "c" "camelCase" = camelCase
                    | otherwise                  = id

createChoice :: String -> (String -> IO ())
createChoice createOp | eitherEq createOp "t" "touch" = create "File" doesFileExist createFile
                      | eitherEq createOp "m" "mkdir" = create "Folder" doesDirectoryExist createDirectory
                      | otherwise                     = putShow

maker createOp sep charCase name = createChoice createOp
                                 $ concat
                                 $ sepChoice sep
                                 $ concat
                                 $ caseChoice charCase
                                 $ tokens name

t = maker "touch" "" ""
m = maker "mkdir" "" ""
o = maker "other" "" ""
