import Data.Char (toLower, toUpper, isSpace, isSeparator, isPunctuation)
import Data.Function (on)
import Data.List (intersperse, groupBy)
import System.IO (writeFile)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist)

-- TODO: coloured output
-- TODO: user prompt to remove illegal characters from file name or otherwise skip operation

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

putId, writeFile' :: String -> IO ()

putId = putStrLn . id
writeFile' s = writeFile s ""

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

hyphenSep, snakeSep, dotSep, extSep :: [String] -> [String]

hyphenSep = intersperse "-"
snakeSep = intersperse "_"
dotSep = intersperse "."
spaceSep = intersperse " "
extSep = putToList . concat

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
-- TODO: use this instead? func1 arg <|> func2 arg <|> func3 arg <|> func4 arg

createFile, createFolder, createSmart :: String -> IO ()

createFile = create "File" doesFileExist writeFile'
createFolder = create "Folder" doesDirectoryExist createDirectory
createSmart s = if '.' `elem` s then createFile s else createFolder s

-- Composition ----------

sepChoice, caseChoice, extChoice :: String -> ([String] -> [String])

sepChoice sep | eitherSep "h" "hyphenSep" = hyphenSep
              | eitherSep "s" "snakeSep"  = snakeSep
              | eitherSep "d" "dotSep"    = dotSep
              | eitherSep "S" "spaceSep"  = spaceSep
              | eitherSep "n" "noSep"     = id
              | otherwise                 = id
                where eitherSep = eitherEq sep

caseChoice charCase | eitherCase "l" "lowerCase" = lowerCase
                    | eitherCase "u" "upperCase" = upperCase
                    | eitherCase "t" "titleCase" = titleCase
                    | eitherCase "c" "camelCase" = camelCase
                    | eitherCase "n" "noCase"    = id
                    | otherwise                  = id
                      where eitherCase = eitherEq charCase

extChoice ext | null ext               = extSep
              | eitherExt "e" "extSep" = extSep
              | otherwise              = sepChoice ext
                where eitherExt = eitherEq ext

createChoice :: String -> (String -> IO ())
createChoice createOp | eitherCreate "t" "touch" = createFile
                      | eitherCreate "m" "mkdir" = createFolder
                      | eitherCreate "s" "smart" = createSmart
                      | otherwise                = putId -- TODO: change to error message later
                        where eitherCreate = eitherEq createOp

maker createOp sep charCase ext name = createChoice createOp $ concat $ dot $ (mapLast $ extChoice ext) $ (mapButLast $ sepChoice sep . caseChoice charCase) $ tokens name

t = maker "touch" "" "" ""
m = maker "mkdir" "" "" ""
s = maker "smart" "" "" ""
o = maker "other" "h" "u" ""
