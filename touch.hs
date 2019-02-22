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

putShow :: String -> IO ()
putShow = putStrLn . show

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

touch s = appendFile s ""
mkDir = createDirectory

create mode name = do
    exists <- touch name
    if exists
       then putStrLn $ show $ "Skipped " ++ title "file" ++ ": '" ++ name ++ "' already exists so it hasn't been changed."
       else do
           touch name
           putShow $ "Created " ++ title "file" ++ ": '" ++ name ++ "'"
--  "file" doesFileExist touch
-- create mode name = do
--     exists <- mode name
--     if exists
--        then putStrLn $ show $ "Skipped " ++ title systemType ++ ": '" ++ name ++ "' already exists so it hasn't been changed."
--        else do
--            create name ""
--            putShow $ "Created " ++ title systemType ++ ": '" ++ name ++ "'"

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

createChoice createOp | eitherEq createOp "t" "touch" = create (touch)
                      | eitherEq createOp "m" "mkdir" = create (touch)
                      | otherwise                     = create (touch)

maker createOp sep charCase name = createChoice createOp
                                 $ concat
                                 $ sepChoice sep
                                 $ concat
                                 $ caseChoice charCase
                                 $ tokens name

t = maker "touch" "h" ""
