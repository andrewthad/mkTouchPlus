{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields, DeriveDataTypeable #-}

import Control.Applicative (liftA2)
import Control.Arrow ((&&&), second)
import Data.Char (isSpace, toLower, toUpper)
import Data.Data (constrFields, Data, toConstr, Typeable)
import Data.Function ((&), on)
import Data.List (groupBy, intercalate, intersperse, span)
import System.IO (writeFile)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory, setCurrentDirectory, getHomeDirectory)

main = input

-- TODO: make -h -v work again

-- Types ----------

data Settings = Settings { ioOperation :: String
                         , separator :: String
                         , characterCase :: String
                         , extensionFormat :: String
                         , sanitisation :: String
                         , name :: [String]
                         } deriving (Data, Show, Typeable)

data Output = Output { home :: String
                     , path :: [String]
                     , name :: String
                     , extension :: String
                     , ioOperation :: String
                     } deriving (Show)

-- Constants ----------

appName, versionNum, readmeUrl, indent :: String

appName = "mkTouch+"
versionNum = "v" ++ show 1.0
readmeUrl = "https://www.com"

indent = replicate 2 ' '

-- Utilities ----------

isSep :: Char -> Bool
isSep c = (c ==) `any` " -_"

eitherEq :: (Eq a) => a -> a -> a -> Bool
eitherEq a = (||) `on` (a ==)

splitWith :: String -> String -> [String]
splitWith l s = let (start, end) = break (`elem` l) s
                in  start : if null end then [] else splitWith l (tail end)

args, tokens :: String -> [String]

args = splitWith ",\n\t"

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

isBlank :: String -> Bool
isBlank = all isSpace

surround :: String -> String -> String -> String
surround s1 s2 s = s1 ++ s ++ s2

surroundSpace, surroundBracket :: String -> String

surroundSpace = surround " " " "
surroundBracket = surround "[" "]"

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

quadApply :: (t1 -> a)
     -> (t2 -> b)
     -> (t3 -> c)
     -> (t4 -> d)
     -> (t1, t2, t3, t4)
     -> (a, b, c, d)
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
interSep sep []           = [""]
interSep sep [""]         = [""]
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

input :: IO ()
input = getContents >>= run . args
        where run x | all isBlank x = noInput
                    | eitherEq x ["-v"] ["--version"] = putStrLn version
                    | eitherEq x ["-h"] ["--help"] = putStrLn help
                    | (a:b:c:d:e:name) <- x = mkTouchPlus Settings
                        { ioOperation     = a
                        , separator       = b
                        , characterCase   = c
                        , extensionFormat = d
                        , sanitisation    = e
                        , name }
                    | name <- x = mkTouchPlus Settings
                        { ioOperation     = ""
                        , separator       = ""
                        , characterCase   = ""
                        , extensionFormat = ""
                        , sanitisation    = ""
                        , name }
              noInput = putStrLn (red "No input")

output :: Output -> IO ()
output (Output {home, path, name, extension, ioOperation})
  | eitherEq ioOperation "e" "echo" = createChoice ioOperation nep
  | otherwise = do
      cd <- getCurrentDirectory
      goHome home
      mkDirPath path
      createChoice ioOperation ne
      setCurrentDirectory cd
   where ne = nameExtDot name extension
         p  = intercalate "/" path ++ "/"
         nep = p ++ ne

goHome :: String -> IO ()
goHome s = do
    home <- getHomeDirectory
    if s == "/" then setCurrentDirectory home else return ()

nameVersion :: String
nameVersion = green (unwords [appName, versionNum])

version :: String
version = "\n" ++ nameVersion

help :: String
help = unlines
    [ ""
    , nameVersion
    , ""
    , "Create one or more files and directory paths, with automatic name formatting."
    , ""
    , "Usage:" ++ surroundSpace (green appName) ++ usage
    , ""
    , indent ++ green "where:"
    , define "ioOperation"     "lorem"
    , define "separator"       "lorem"
    , define "characterCase"   "lorem"
    , define "extensionFormat" "lorem"
    , define "sanitisation"    "lorem"
    , define "name"            "lorem"
    , ""
    , "For more help, open the readme in your browser:"
    , ""
    , green readmeUrl ]
      where define name explanation = concat [ [1,2] >> indent , take 24 (blue name ++ repeat ' ') , surroundSpace $ green ":" , explanation ]
            settings = constrFields . toConstr $ Settings "" "" "" "" "" [""]
            usage = intercalate (green ",") $ (\ s -> surroundBracket (blue s)) <$> settings

-- Composition ----------

sepChoice, caseChoice, extChoice, sanitiseChoice :: String -> ([String] -> [String])

sepChoice sep | eitherSep "h" "hyphenSep" = hyphenSep
              | eitherSep "s" "snakeSep"  = snakeSep
              | eitherSep "d" "dotSep"    = dotSep
              | eitherSep "S" "spaceSep"  = spaceSep
              | eitherSep "n" "noSep"     = id
              | otherwise                 = hyphenSep
                where eitherSep = eitherEq sep

caseChoice characterCase | eitherCase "l" "lowerCase" = lowerCase
                         | eitherCase "u" "upperCase" = upperCase
                         | eitherCase "t" "titleCase" = titleCase
                         | eitherCase "c" "camelCase" = camelCase
                         | eitherCase "n" "noCase"    = id
                         | otherwise                  = id
                           where eitherCase = eitherEq characterCase

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

mkTouchPlus :: Settings -> IO ()
mkTouchPlus (Settings { ioOperation
                      , separator
                      , characterCase
                      , extensionFormat
                      , sanitisation
                      , name }) = composition
    where composition = creator $ quadApply homeF pathF nameF extF <$> pathNameExt <$> name
          homeF = dropWhile isSpace
          pathF = \s -> noNulls $ tokenSepSanCase <$> splitWith "/" s
          nameF = tokenSepSanCase
          extF  = tokenApply $ extChoice extensionFormat . sanitiseChoice sanitisation
          creator x = putLineSurround $ sequence_ [output (Output { home = h
                                                                  , path = p
                                                                  , name = n
                                                                  , extension = e
                                                                  , ioOperation }) | (h,p,n,e) <- x]
          tokenApply f = concat . f <$> tokens
          tokenSepSanCase = tokenApply $ sepChoice separator . sanitiseChoice sanitisation . caseChoice characterCase
