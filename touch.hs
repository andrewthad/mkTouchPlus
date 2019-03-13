import Control.Arrow   -- (&&&)
import Control.Applicative (liftA2)
import Data.Char (toLower, toUpper)
import Data.Function (on)
import Data.List (intersperse, groupBy, intercalate)
import System.IO (writeFile)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, setCurrentDirectory)

-- main = maker "" "" "" "" ""

-- TODO: create folders if they dont exist automatically. e.g. s "test/this/thing.txt". You will need to modify the create function
-- But does ../test.txt work properly?
-- Does smart work with path/folder/ ?

-- TODO: if no string is provided for filename/s input, then it should accept from stInput. This allows piping?

-- TODO: use this instead of nested ifs? func1 arg <|> func2 arg <|> func3 arg <|> func4 arg

-- TODO: tidy up all code at the end

-- Utilities ----------

isToken :: Char -> Bool
isToken c = (c ==) `any` " -_"

eitherEq :: (Eq a) => a -> a -> a -> Bool
eitherEq a = (||) `on` (a ==)

splitWith :: Char -> String -> [String]
splitWith c s = let (start, end) = break (== c) s
               in  start : if null end then [] else splitWith c (tail end)

tokens :: String -> [String]
tokens s = if s' == "" then [] else word : tokens rest
  where s' = dropWhile isToken s
        (word, rest) = break isToken s'

multi :: String -> [String]
multi = splitWith ','

putId, writeFile' :: String -> IO ()

putId = putStrLn . id
writeFile' s = writeFile s ""

putToList :: a -> [a]
putToList x = [x]

anyEq :: Eq a => [a] -> [a] -> Bool
anyEq x y = any id $ liftA2 (==) x y

-- Tokenisation ----------

notNull :: [a] -> Bool
notNull = not . null

lNotNull :: [String] -> [String]
lNotNull = filter notNull

sections :: String -> [String]
sections = lNotNull . splitWith '.'

-- nameExt :: (String, String) -> String -> (String, String)
-- nameExt (a,b) ""  = (a,b)
-- nameExt (a,b) [c] = ([c],b)
-- nameExt (a,b) s   = divy (a,b) (reverse s)
--     where divy :: (String, String) -> String -> (String, String)
--           divy (a,b) ""       = (a,b)
--           divy (a,b) x@(c:cs) = if '.' `elem` x
--                                    then if c == '.'
--                                            then (reverse x,b)
--                                            else divy (a,c:b) cs
--                                    else (reverse x,b)

-- pathFile :: String -> (String, String)
-- pathFile = foldr splitPath ("","")
--     where splitPath c = if c == 
--
-- import Data.List       -- intercalate
-- import Data.List.Split -- splitOn
-- breakOnLast :: Eq a => a -> [a] -> ([a], [a])
-- breakOnLast x = (intercalate x . init &&& last) . splitOn x

splitLast :: Char -> String -> (String, String)
splitLast c = (intercalate [c] . init &&& last) . splitWith c

nameExt, pathFile :: String -> (String, String)
nameExt = splitLast '.'
pathFile = splitLast '/'

-- pathFile x = (intercalate x . init &&& last) . splitWith (head x)

fNameExt :: (a1 -> a2) -> (b1 -> b2) -> (a1, b1) -> (a2, b2)
fNameExt fa fb (name, ext) = (fa name, fb ext)

nameExtDot :: (String, String) -> String
nameExtDot ("","") = ""
nameExtDot (a,"") = a
nameExtDot ("",b) = b
nameExtDot (a,b) = if last a == '.' then a ++ b else a ++ "." ++ b

-- Separators ----------

hyphenToken, snakeToken, dotToken, extToken :: [String] -> [String]

hyphenToken = interSep "-"
snakeToken = interSep "_"
dotToken = interSep "."
spaceToken = interSep " "
extToken = putToList . concat

interSep :: String -> [String] -> [String]
interSep sep [x]          = [x]
interSep sep (x:xs@(y:z)) = if [last x, head y] `anyEq` "./"
                               then x : interSep sep xs
                               else x : sep : interSep sep xs

dot :: [[String]] -> [String]
dot = foldr (\x y -> if y == [] then x ++ y else x ++ ["."] ++ y) []

-- Cases ----------

title :: String -> String
title (c:cs) = toUpper c : cs

lowerCase, upperCase, titleCase, camelCase :: [String] -> [String]

lowerCase = map $ map toLower
upperCase = map $ map toUpper

titleCase = map title

camelCase [] = []
camelCase [x] = [x]
camelCase (x:xs) = x : map title xs

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

ansiCode :: Int -> String
ansiCode n = "\x1b["  ++ show n ++ "m"

reset :: String
reset = ansiCode 0

colorStr :: Int -> String -> String
colorStr n s = ansiCode n ++ s ++ reset

toGreen, toRed :: String -> String
toGreen = colorStr 32
toRed = colorStr 31
toMagenta = colorStr 35

msg :: String -> (String -> String) -> String -> String -> IO ()
msg form color op s = putId $ color (take 16 $ title form ++ " " ++ op ++ ":" ++ repeat ' ') ++ s

createMsg, skipMsg :: String -> String -> IO ()

createMsg op s = msg op toGreen "Created" s
skipMsg op s = msg op toRed "Skipped" s

create :: String -> (String -> IO Bool) -> (String -> IO ()) -> String -> IO ()
create form existF makeF s = if s == ""
                                then prompt form makeF
                                else do exists <- existF s
                                        if exists
                                           then skip form s
                                           else make form makeF s

skip :: String -> String -> IO ()
skip form s = skipMsg form ("The " ++ form ++ " " ++ toMagenta s ++ " already exists, but it hasn't been overwritten by this operation")

make :: String -> (String -> IO ()) -> String -> IO ()
make form makeF s = do
    makeF s
    createMsg form (toMagenta s)

prompt :: String -> (String -> IO ()) -> IO ()
prompt form makeF = do
    putStrLn "Please enter a filename:"
    s <- getLine
    make form makeF s

mkDirp :: String -> IO ()
mkDirp path = (\x -> createDirectory x >> setCurrentDirectory x) `mapM_` (splitWith '/' path)

createFile, createFolder, createSmart :: String -> IO ()

createFile = create "file" doesFileExist writeFile'
createFolder = create "folder" doesDirectoryExist createDirectory
createSmart s = if '.' `elem` s then createFile s else createFolder s

-- Composition ----------

tokenChoice, caseChoice, extChoice :: String -> ([String] -> [String])

tokenChoice token | eitherToken "h" "hyphenToken" = hyphenToken
              | eitherToken "s" "snakeToken"  = snakeToken
              | eitherToken "d" "dotToken"    = dotToken
              | eitherToken "S" "spaceToken"  = spaceToken
              | eitherToken "n" "noToken"     = id
              | otherwise                 = id
                where eitherToken = eitherEq token

caseChoice charCase | eitherCase "l" "lowerCase" = lowerCase
                    | eitherCase "u" "upperCase" = upperCase
                    | eitherCase "t" "titleCase" = titleCase
                    | eitherCase "c" "camelCase" = camelCase
                    | eitherCase "n" "noCase"    = id
                    | otherwise                  = id
                      where eitherCase = eitherEq charCase

extChoice ext | null ext               = extToken
              | eitherExt "e" "extToken" = extToken
              | otherwise              = tokenChoice ext
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

maker createOp token charCase ext san name = {- (createChoice createOp) `mapM_` -} ({- nameExtDot . -} fNameExt
    ({- concat . tokenChoice token . lNotNull . sanitiseChoice san . caseChoice charCase <$> -} tokens)
    (concat . extChoice ext . lNotNull . sanitiseChoice san <$> tokens)
    . nameExt <$> multi name)

o = maker "" "" "" "" ""
e = maker "echo" "h" "u" "" ""
s = maker "smart" "h" "c" "l" ""
