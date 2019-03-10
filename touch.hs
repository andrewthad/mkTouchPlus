import Data.Char (toLower, toUpper)
import Data.Function (on)
import Data.List (intersperse, groupBy)
import System.IO (writeFile)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist)

-- main = maker "" "" "" "" ""

-- TODO: createSmart should check ext == "" (in fileExt) instead

-- TODO: coloured output
-- leave this until end. if no other dependencies, hand-code it
-- use terminal colour codes that change depending on the user's theme
-- have two arguments for the colour codes to use. feed these to ioChoice

-- TODO next: multi support
-- multi = groupStr ','
-- tokens = words <$> sections s <$> multi

-- TODO: instead of skipping a file or folder that already exists, an alternative option is to number them automatically.


-- TODO: writeFile' with path/test.txt already works. just add / as a separator so that you can strip whitespace around it?
-- But does ../test.txt work properly?
-- Does smart work with path/folder/ ?

-- TODO later: add cp and mv functions

-- TODO: automatic numbering of identicle files as an option. e.g. file.txt -> file2.txt etc. works great with multiplied files e.g. file.txt *3 -> file.txt, file-2.txt, file-3.txt

-- Utilities ----------

isSep :: Char -> Bool
isSep c = (c ==) `any` " -_"

eitherEq :: (Eq a) => a -> a -> a -> Bool
eitherEq a = (||) `on` (a ==)

groupStr :: Char -> String -> [String]
groupStr c s = let (start, end) = break (== c) s
               in  start : if null end then [] else groupStr c (tail end)

seps :: String -> [String]
seps s = if s' == "" then [] else word : seps rest
  where s' = dropWhile isSep s
        (word, rest) = break isSep s'

mapButLast, mapButFirst :: (a -> a) -> [a] -> [a]

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

-- notNull :: Foldable t => ta -> Bool
notNull = not . null

lNotNull :: [String] -> [String]
lNotNull = filter notNull

sections :: String -> [String]
sections = lNotNull . groupStr '.'

-- TODO: tidy this up:
-- TODO: make it accept just a string. use a massive where clause
nameExt :: (String, String) -> String -> (String, String)
nameExt (a,b) ""  = (a,b)
nameExt (a,b) [c] = ([c],b)
nameExt (a,b) s   = divy (a,b) (reverse s)
    where divy :: (String, String) -> String -> (String, String)
          divy (a,b) ""       = (a,b)
          divy (a,b) x@(c:cs) = if '.' `elem` x
                                   then if c == '.'
                                           then (reverse x,b)
                                           else divy (a,c:b) cs
                                   else (reverse x,b)

fNameExt :: (a1 -> a2) -> (b1 -> b2) -> (a1, b1) -> (a2, b2)
fNameExt fa fb (name, ext) = (fa name, fb ext)

nameExtDot :: (String, String) -> String
nameExtDot ("","") = ""
nameExtDot (a,"") = a
nameExtDot ("",b) = b
nameExtDot (a,b) = a ++ "." ++ b

tokens :: String -> [[String]]
tokens s = seps <$> sections s

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

-- Sanitisation ----------

-- exclude, include :: String -> String -> String

exclude s = map $ filter (not . (`elem` s))
include s = map $ filter (`elem` s)

control, nbsp, spaces, punctuation, separators, numbers, capitals, letters, unixEx, macEx, windowsEx, sensibleEx, conservativeIn :: String

control = "\NUL" ++ ['\SOH'..'\US'] ++ "\DEL"
nbsp = "\255"
spaces = " " ++ nbsp
punctuation = ['\33'..'\44'] ++ ['\58'..'\64'] ++ ['\91'..'\94'] ++ "`" ++ ['\123'..'\126']
separators = "-_"
numbers = ['\48'..'\57']
capitals = ['\65'..'\90']
letters = ['\97'..'\122']

unixEx = "\NUL/"
macEx = unixEx ++ ":"
windowsEx = control ++ "\\?%*:|\"<>."
sensibleEx = control ++ spaces ++ punctuation
conservativeIn = separators ++ numbers ++ capitals ++ letters

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

-- sanitiseChoice :: String -> (String -> String)
sanitiseChoice san | eitherSan "u" "unix"         = exclude unixEx
                   | eitherSan "w" "windows"      = exclude windowsEx
                   | eitherSan "m" "mac"          = exclude macEx
                   | eitherSan "s" "sensible"     = sensible
                   | eitherSan "c" "conservative" = include conservativeIn
                   | otherwise                    = sensible
                     where eitherSan = eitherEq san
                           sensible  = exclude sensibleEx

createChoice :: String -> (String -> IO ())
createChoice createOp | eitherCreate "t" "touch" = createFile
                      | eitherCreate "m" "mkdir" = createFolder
                      | eitherCreate "s" "smart" = createSmart
                      | eitherCreate "e" "echo"  = putId
                      | otherwise                = createSmart
                        where eitherCreate = eitherEq createOp

maker createOp sep charCase ext san name = createChoice createOp $ nameExtDot $ fNameExt (concat . dot . (map $ sepChoice sep . lNotNull {- . sanitiseChoice san -} . caseChoice charCase) <$> tokens) (concat . extChoice ext . lNotNull {- . sanitiseChoice san -} <$> seps) $ nameExt ("","") name

-- t = maker "touch" "" "" "" ""
-- m = maker "mkdir" "" "" "" ""
-- s = maker "smart" "" "" "" ""
-- o = maker "echo" "h" "u" "" ""
o = maker "echo" "h" "u" "l" ""
