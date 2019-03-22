import Control.Applicative (liftA2)
import Control.Arrow ((&&&), second)
import Data.Char (isSpace, toLower, toUpper)
import Data.Function ((&), on)
import Data.List (groupBy, intercalate, intersperse, span)
import System.IO (writeFile)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory, setCurrentDirectory, getHomeDirectory)

main = input "" "" "" "" ""

b = maker "" "h" "" "" ""
o = maker "" "" "" "" ""
e = maker "echo" "h" "u" "" ""
s = maker "smart" "h" "c" "l" ""

-- Constants ----------

name, version, readme, indent :: String

name = "Nice Touch"
version = "v" ++ show 1.0
readme = "https://www.com"

indent = replicate 2 ' '

-- Utilities ----------

isSep :: Char -> Bool
isSep c = (c ==) `any` " -_"

eitherEq :: (Eq a) => a -> a -> a -> Bool
eitherEq a = (||) `on` (a ==)

splitWith :: String -> String -> [String]
splitWith l s = let (start, end) = break (`elem` l) s
                in  start : if null end then [] else splitWith l (tail end)

multi, tokens :: String -> [String]

multi = splitWith ",\n\t"

tokens s = if s' == "" then [] else word : tokens rest
  where s' = dropWhile isSep s
        (word, rest) = break isSep s'

anyEq :: Eq a => [a] -> [a] -> Bool
anyEq x y = any id $ liftA2 (==) x y

shrinkTo :: Int -> String -> String
shrinkTo n s = if length s > n
                 then let half = floor $ fromIntegral ((n - 3) `div` 2)
                          left = take half s
                          right = reverse $ take half $ reverse s
                      in  left ++ "..." ++ right
                 else s

shrink :: String -> String
shrink = shrinkTo 21

twoNL :: String
twoNL = "\n\n"

isBlank :: String -> Bool
isBlank = all isSpace

-- Tokenisation ----------

notNull :: [a] -> Bool
notNull = not . null

noNulls :: [String] -> [String]
noNulls = filter notNull

splitLast :: String -> String -> (String, String)
splitLast s = (intercalate s . init &&& last) . splitWith s

homePath, nameExt, pathFile :: String -> (String, String)

homePath = span (\ c -> eitherEq c ' ' '/')

nameExt s = let dot = '.' in if dot `elem` s then splitLast [dot] s else (s,"")

pathFile = splitLast "/"

pathNameExt :: String -> (String, String, String, String)
pathNameExt s = (\ x -> nameExt `second` pathFile x) `second` homePath s
                & \ (a,(b,(c,d))) -> (a,b,c,d)

quadApply :: (t1 -> a) -> (t2 -> b) -> (t3 -> c) -> (t4 -> d) -> (t1, t2, t3, t4) -> (a, b, c, d)
quadApply fa fb fc fd (a, b, c, d) = (fa a, fb b, fc c, fd d)

nameExtDot :: String -> String -> String
nameExtDot "" "" = ""
nameExtDot a "" = a
nameExtDot "" b = b
nameExtDot a b = if last a == '.' then a ++ b else a ++ "." ++ b

-- Separators ----------

hyphenSep, snakeSep, dotSep, extSep :: [String] -> [String]

hyphenSep = interSep "-"
snakeSep = interSep "_"
dotSep = interSep "."
spaceSep = interSep " "
extSep = (\x -> [x]) . concat

interSep :: String -> [String] -> [String]
interSep sep [x]          = [x]
interSep sep (x:xs@(y:z)) = if [last x, head y] `anyEq` "./"
                               then x : interSep sep xs
                               else x : sep : interSep sep xs

-- Cases ----------

title :: String -> String
title (c:cs) = toUpper c : cs

twoMap :: (a -> b) -> [[a]] -> [[b]]
twoMap = fmap . fmap

lowerCase, upperCase, titleCase, camelCase :: [String] -> [String]

lowerCase = twoMap toLower
upperCase = twoMap toUpper

titleCase = fmap title

camelCase [] = []
camelCase [x] = [x]
camelCase (x:xs) = x : fmap title xs

-- Sanitisation ----------

clude :: (Foldable t, Eq a) => (Bool -> Bool) -> t a -> [[a]] -> [[a]]
clude neg s = fmap $ filter (neg . (`elem` s))

include, exclude :: (Foldable t, Eq a) => t a -> [[a]] -> [[a]]

include = clude id
exclude = clude not

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

unix, windows, mac, sensible, conservative :: [String] -> [String]

unix = exclude unixEx
windows = exclude windowsEx
mac = exclude macEx
sensible = exclude sensibleEx
conservative = include conservativeIn


-- IO ----------

putLine :: IO ()
putLine = putStr "\n"

putLineSurround :: IO a -> IO ()
putLineSurround f = putLine >> f >> putLine

ansiCode :: Int -> String
ansiCode n = "\x1b["  ++ show n ++ "m"

reset :: String
reset = ansiCode 0

colored :: Int -> String -> String
colored n s = ansiCode n ++ s ++ reset

red, green, blue :: String -> String

red = colored 31
green = colored 32
blue = colored 34

createSmart :: String -> IO ()
createSmart "" = return ()
createSmart s = if '.' `elem` s then createFile s else createDir s

errorMsg :: String -> String
errorMsg s = red $ indent ++ "-- " ++ s

skipMsg :: String

skipMsg = errorMsg "Not touched."

fileMsg, dirMsg :: (String -> String) -> String -> String

fileMsg color = color . shrink
dirMsg color = fileMsg color . (++ "/")

fileSuccess, dirSuccess, dirFailure :: String -> String

fileSuccess = fileMsg blue
dirSuccess = dirMsg green
dirFailure = dirMsg red

createOutput :: (String -> IO Bool) -> (String -> IO a) -> (String -> String) -> String -> IO ()
createOutput existF createF successMsgF s = existCheck existF exists notExists s
    where exists = putStr (red s) >> putStrLn skipMsg
          notExists = createF s >> putStrLn (successMsgF s)

createFile, createDir :: String -> IO ()

createFile "" = return ()
createFile s = createOutput doesFileExist (\s -> writeFile s "") fileSuccess s

createDir "" = return ()
createDir s = createOutput doesDirectoryExist createDirectory dirSuccess s

existCheck :: Monad m => (t -> m Bool) -> m b -> m b -> t -> m b
existCheck existF exists notExists s = do
    yes <- existF s
    if yes then exists else notExists

mkDirPath :: [String] -> IO ()
mkDirPath = mapM_ mkStep

mkStep, skip, make :: String -> IO ()

mkStep "" = return ()
mkStep ".." = parentStep
mkStep s = existCheck doesDirectoryExist (skip s) (make s) s

skip "" = return ()
skip s = do
    setCurrentDirectory s
    putStr (dirFailure s)

make "" = return ()
make s = do
    createDirectory s
    setCurrentDirectory s
    putStr (dirSuccess s)

parentStep :: IO ()
parentStep = do
    cd <- getCurrentDirectory
    let parent = fst $ pathFile cd
        ifExists = setCurrentDirectory parent >> putStr "../"
    existCheck doesDirectoryExist ifExists (return ()) parent

input :: String -> String -> String -> String -> String -> IO ()
input op sep char ext san = do
    putStrLn (blue "Enter a path:")
    s <- getLine
    maker op sep char ext san s

output :: String -> [String] -> String -> String -> String -> IO ()
output h p n e op | eitherEq op "e" "echo" = createChoice op nepS
                  | otherwise = do
                      cd <- getCurrentDirectory
                      goHome h
                      mkDirPath p
                      createChoice op neS
                      setCurrentDirectory cd
                   where neS = nameExtDot n e
                         pS  = intercalate "/" p ++ "/"
                         nepS = pS ++ neS

goHome :: String -> IO ()
goHome s = do
    home <- getHomeDirectory
    if s == "/" then setCurrentDirectory home else return ()

help :: String
help = concat [ "\n"
              , green (name ++ " " ++ version)
              , twoNL
              , "For help, open the readme in your browser:"
              , twoNL
              , blue readme
              , "\n" ]

-- Composition ----------

sepChoice, caseChoice, extChoice, sanitiseChoice :: String -> ([String] -> [String])

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

sanitiseChoice san = noNulls . sanitiser san
    where eitherSan = eitherEq san
          sanitiser san | eitherSan "u" "unix"         = unix
                        | eitherSan "w" "windows"      = windows
                        | eitherSan "m" "mac"          = mac
                        | eitherSan "s" "sensible"     = sensible
                        | eitherSan "c" "conservative" = conservative
                        | otherwise                    = sensible

createChoice :: String -> (String -> IO ())
createChoice createOp | eitherCreate "t" "touch" = createFile
                      | eitherCreate "m" "mkdir" = createDir
                      | eitherCreate "s" "smart" = createSmart
                      | eitherCreate "e" "echo"  = putStrLn
                      | otherwise                = createSmart
                        where eitherCreate = eitherEq createOp

maker :: String -> String -> String -> String -> String -> String -> IO ()
maker op sep char ext san name | eitherEq name "-h" "--help" = putStrLn help
                               | isBlank name = input op sep char ext san
                               | otherwise = creator $ quadApply homeF pathF nameF extF <$> pathNameExt <$> multi name
    where homeF = dropWhile isSpace
          pathF = \s -> noNulls $ tokenSepSanCase <$> splitWith "/" s
          nameF = tokenSepSanCase
          extF  = tokenApply $ extChoice ext . sanitiseChoice san
          creator x = putLineSurround $ sequence_ [output h p n e op | (h,p,n,e) <- x]
          tokenApply f = concat . f <$> tokens
          tokenSepSanCase = tokenApply $ sepChoice sep . sanitiseChoice san . caseChoice char
