import Control.Arrow
import Data.Function ((&))
-- import Control.Monad
import Control.Applicative (liftA2)
import Data.Char (toLower, toUpper)
import Data.Function (on)
import Data.List (intersperse, groupBy, intercalate)
import System.IO (writeFile)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, setCurrentDirectory, getCurrentDirectory)

main = input "" "" "" "" ""

-- TODO: make multi accept tabs and linebreaks also. Make it work with the different types of line endings properly
-- TODO: make a function that ellipsises long filenames from middle. e.g. this-is-a-l...ame-it-is.txt

-- Utilities ----------

-- version :: Decimal
version = 1.0

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

putToList :: a -> [a]
putToList x = [x]

anyEq :: Eq a => [a] -> [a] -> Bool
anyEq x y = any id $ liftA2 (==) x y

shorten n s = if length s > n
                 then let half = floor $ fromIntegral ((n - 3) `div` 2)
                          left = take half s
                          right = reverse $ take half $ reverse s
                      in  left ++ "..." ++ right
                 else s

-- Tokenisation ----------

notNull :: [a] -> Bool
notNull = not . null

lNotNull :: [String] -> [String]
lNotNull = filter notNull

sections :: String -> [String]
sections = lNotNull . splitWith '.'

splitLast :: Char -> String -> (String, String)
splitLast c = (intercalate [c] . init &&& last) . splitWith c

nameExt, pathFile :: String -> (String, String)
nameExt s = let sep = '.' in if sep `elem` s then splitLast sep s else (s,"")
pathFile = splitLast '/'

pathNameExt :: String -> (String, String, String)
pathNameExt s = nameExt `second` pathFile s & \(a,(b,c)) -> (a,b,c)

triApply :: (t1 -> a) -> (t2 -> b) -> (t3 -> c) -> (t1, t2, t3) -> (a, b, c)
triApply fa fb fc (a, b, c) = (fa a, fb b, fc c)

modSplit :: (a1 -> a2) -> (b1 -> b2) -> (a1, b1) -> (a2, b2)
modSplit fa fb (name, ext) = (fa name, fb ext)

splitDot :: (String, String) -> String
splitDot ("","") = ""
splitDot (a,"") = a
splitDot ("",b) = b
splitDot (a,b) = if last a == '.' then a ++ b else a ++ "." ++ b

nameExtDot :: String -> String -> String
nameExtDot "" "" = ""
nameExtDot a "" = a
nameExtDot "" b = b
nameExtDot a b = if last a == '.' then a ++ b else a ++ "." ++ b

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

-- TODO: remove the 'to'
-- toGreen, toRed :: String -> String
toRed = colorStr 31
toGreen = colorStr 32
toBlue = colorStr 34

createSmart "" = return ()
createSmart s = if '.' `elem` s then createFile s else createDir s

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

-- createChoice :: String -> (String -> IO ())
createChoice createOp | eitherCreate "t" "touch" = createFile
                      | eitherCreate "m" "mkdir" = createDirectory
                      | eitherCreate "s" "smart" = createSmart
                      | eitherCreate "e" "echo"  = putStrLn
                      | otherwise                = createSmart
                        where eitherCreate = eitherEq createOp

skipMsg :: String -> (String -> String) -> String -> String
skipMsg kind color s = toRed (s ++ "\n\nThe " ++ kind) ++ " " ++ color s ++ " " ++ toRed "already exists, so it hasn't been touched."

createFile s = do
    exists <- doesFileExist s
    if not exists
       then writeFile s "" >> putStrLn (toBlue s)
       else putStrLn $ skipMsg "file" toBlue s

createDir s = do
    exists <- doesDirectoryExist s
    if not exists
       then createDirectory s >> putStrLn (toGreen s)
       else putStrLn $ skipMsg "folder" toGreen s

mkDirp [""] = return ()
mkDirp [x] = mkCheck x [""]
mkDirp (x:xs) = mkCheck x xs

mkCheck x xs = do exists <- doesDirectoryExist x
                  if not exists
                     then mkStep x xs
                     else skipStep x xs

skipStep "" [""] = return ()
skipStep x [""] = skipMk x >> return ()
skipStep x z = skipMk x >> mkDirp z

skipMk x = setCurrentDirectory x >> putStr (toRed $ x ++ "/")

mkStep "" [""] = return ()
mkStep x [""] = createStep x
mkStep x [y] = createStep x >> putStr (toGreen $ x ++ "/") >> mkStep y [""]
mkStep x (y:ys) = createStep x >> putStr (toGreen $ x ++ "/") >> mkStep y ys

createStep x = createDirectory x >> setCurrentDirectory x

input op token char ext san = do
    putStrLn "Enter a path:"
    s <- getLine
    maker op token char ext san s

output p n e op = let neS = nameExtDot n e
                      pS  = intercalate "/" p ++ "/"
                      nepS = pS ++ neS
                  in if op == "e" || op == "echo"
                        then createChoice op nepS
                        else do
                            origDir <- getCurrentDirectory
                            putStrLn ""
                            mkDirp p
                            createChoice op neS
                            putStrLn ""
                            setCurrentDirectory origDir
                            return ()

maker op token char ext san ""   = input op token char ext san
maker op token char ext san name = if name == "-h" || name == "--help"
                                      then putStrLn $ "\n" ++ toGreen ("Nice Touch v" ++ show version) ++ "\n\nFor help, open the readme in your browser:\n\n" ++ toBlue "https://www.com" ++ "\n"
                                      else creator $ triApply pathF nameF extF <$> pathNameExt <$> multi name
    where pathF = lNotNull . sanitiseChoice san . caseChoice char <$> splitWith '/'
          nameF = concat . tokenChoice token . lNotNull . sanitiseChoice san . caseChoice char <$> tokens
          extF  = concat . extChoice ext . lNotNull . sanitiseChoice san <$> tokens
          creator x = sequence_ [output p n e op | (p,n,e) <- x]

o = maker "" "" "" "" ""
e = maker "echo" "h" "u" "" ""
s = maker "smart" "h" "c" "l" ""
