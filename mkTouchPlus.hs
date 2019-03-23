{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields, DeriveDataTypeable #-}

import Control.Applicative (liftA2)
import Control.Arrow ((&&&), second)
import Data.Char (isSpace, toLower, toUpper)
import Data.Data (constrFields, Data, toConstr, Typeable)
import Data.Function ((&), on)
import Data.List (groupBy, intercalate, intersperse, span)
import System.IO (writeFile)
import System.Directory ( createDirectory
                        , doesDirectoryExist
                        , doesFileExist
                        , getCurrentDirectory
                        , setCurrentDirectory
                        , getHomeDirectory )

main = input

-- TODO: option to not revert back to original directory at end of output
-- TODO: test all examples for if they work and are coloured properly
-- TODO: publish on github and change readme url

-- Types ----------

data Settings = Settings { ioOperation     :: String
                         , separator       :: String
                         , characterCase   :: String
                         , extensionFormat :: String
                         , sanitisation    :: String
                         , name            :: [String]
                         } deriving (Data, Show, Typeable)

data Output = Output { home        :: String
                     , path        :: [String]
                     , name        :: String
                     , extension   :: String
                     , ioOperation :: String
                     } deriving (Show)

-- Constants ----------

appName, versionNum, readmeUrl, indent :: String

appName    = "mkTouch+"
versionNum = "v" ++ show 1.0
readmeUrl  = "https://www.com"

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
  where s'           = dropWhile isSep s
        (word, rest) = break isSep s'

anyEq :: Eq a => [a] -> [a] -> Bool
anyEq x y = any id $ liftA2 (==) x y

shrinkTo :: Int -> String -> String
shrinkTo n s = if length s > n
                 then let half  = floor $ fromIntegral ((n - 3) `div` 2)
                          left  = take half s
                          right = reverse $ take half $ reverse s
                      in  left ++ "..." ++ right
                 else s

shrink :: String -> String
shrink = shrinkTo 21

isBlank :: String -> Bool
isBlank = all isSpace

surround :: String -> String -> String -> String
surround s1 s2 s = s1 ++ s ++ s2

spaceSurround :: String -> String
spaceSurround = surround " " " "
lineSurround  = surround "\n" "\n" . (indent ++)

indentAll :: [String] -> [String]
indentAll = map (\ s -> if isBlank s then s else indent ++ s)

head' :: String -> String
head' ""      = ""
head' (c:cs)  = [c]

duplicate :: Int -> String -> String
duplicate n s = [1..n] >> s

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
whitespaceSep = interSep " "
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

control     = "\NUL" ++ ['\SOH'..'\US'] ++ "\DEL"
nbsp        = "\255"
spaces      = " " ++ nbsp
punctuation = concat [ ['\33'..'\44']
                     , ['\58'..'\64']
                     , ['\91'..'\94']
                     , "`"
                     , ['\123'..'\126'] ]
separators  = "-_"
numbers     = ['\48'..'\57']
capitals    = ['\65'..'\90']
letters     = ['\97'..'\122']

unixEx         = "\NUL/"
macEx          = unixEx ++ ":"
windowsEx      = control ++ "\\?%*:|\"<>."
sensibleEx     = control ++ spaces ++ punctuation
conservativeIn = concat [separators, numbers, capitals, letters]

unix, windows, mac, sensible, conservative :: [String] -> [String]

unix         = exclude unixEx
windows      = exclude windowsEx
mac          = exclude macEx
sensible     = exclude sensibleEx
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

red   = colored 31
green = colored 32
blue  = colored 34

smartCreate :: String -> IO ()
smartCreate "" = return ()
smartCreate s  = if '.' `elem` s then fileCreate s else dirCreate s

errorMsg :: String -> String
errorMsg s = red $ indent ++ "-- " ++ s

skipMsg :: String

skipMsg = errorMsg "Not touched."

dirPrint :: (String -> String) -> String -> String
dirPrint color = color . (++ "/")

dirGreen, dirRed :: String -> String

dirGreen = dirPrint green
dirRed = dirPrint red

fileMsg, dirMsg :: (String -> String) -> String -> String

fileMsg color = color . shrink
dirMsg color  = fileMsg (dirPrint color)

fileSuccess, dirSuccess, dirFailure :: String -> String

fileSuccess = fileMsg blue
dirSuccess  = dirMsg green
dirFailure  = dirMsg red

createOutput :: (String -> IO Bool) -> (String -> IO a) -> (String -> String) -> String -> IO ()
createOutput existF createF successMsgF s = existCheck existF exists notExists s
    where exists    = putStr (red s) >> putStrLn skipMsg
          notExists = createF s >> putStrLn (successMsgF s)

fileCreate, dirCreate :: String -> IO ()

fileCreate "" = return ()
fileCreate s  = createOutput doesFileExist (\s -> writeFile s "") fileSuccess s

dirCreate "" = return ()
dirCreate s  = createOutput doesDirectoryExist createDirectory dirSuccess s

existCheck :: Monad m => (t -> m Bool) -> m b -> m b -> t -> m b
existCheck existF exists notExists s = do
    yes <- existF s
    if yes then exists else notExists

mkDirPath :: [String] -> IO ()
mkDirPath = mapM_ mkStep

mkStep, skip, make :: String -> IO ()

mkStep ""   = return ()
mkStep ".." = parentStep
mkStep s    = existCheck doesDirectoryExist (skip s) (make s) s

skip "" = return ()
skip s  = do
    setCurrentDirectory s
    putStr (dirFailure s)

make "" = return ()
make s  = do
    createDirectory s
    setCurrentDirectory s
    putStr (dirSuccess s)

parentStep :: IO ()
parentStep = do
    cd <- getCurrentDirectory
    let parent   = fst $ pathFile cd
        ifExists = setCurrentDirectory parent >> putStr "../"
    existCheck doesDirectoryExist ifExists (return ()) parent

input :: IO ()
input = getContents >>= run . args
        where run x | all isBlank x = noInput
                    | x `anyEq` ["-v", "--v", "--version"] = putStrLn version
                    | x `anyEq` ["-h", "--h", "--help"]    = putStrLn help
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
              noInput = putStrLn (lineSurround $ red "No input")

output :: Output -> IO ()
output (Output {home, path, name, extension, ioOperation})
  | eitherEq ioOperation "e" "echo" = ioChoice ioOperation nep
  | isBlank name && notNull extension = output
      (Output { home      = home
              , path      = path
              , name      = dotFile extension
              , extension = ""
              , ioOperation } )
  | otherwise = do
      putStr indent
      cd <- getCurrentDirectory
      goHome home
      mkDirPath path
      ioChoice ioOperation ne
      setCurrentDirectory cd
   where dotFile = ("." ++)
         ne      = nameExtDot name extension
         p       = intercalate "/" path ++ "/"
         nep     = p ++ ne

goHome :: String -> IO ()
goHome s = do
    home <- getHomeDirectory
    if s == "/" then setCurrentDirectory home else return ()

nameVersion :: String
nameVersion = green $ unwords [appName, versionNum]

version :: String
version = lineSurround $ nameVersion

help :: String
help = unlines $ indentAll
    [ lineSurround nameVersion
    , description
    , lineSurround $ heading "usage" ----------
    , command usage
    , orUsage
    , command $ tag "name"
    , orUsage
    , command flags
    , ""
    , indent ++ "Where:"
    , define "ioOperation"     ioOptions
    , define "separator"       sepOptions
    , define "characterCase"   caseOptions
    , define "extensionFormat" extOptions
    , define "sanitisation"    sanOptions
    , definition "name"        nameDefinition
    , heading "examples" ----------
    , ""
    , blue "create file.txt"
    , green "create folder"
    , dirGreen "create/a" ++ dirGreen "path"
    , dirGreen "create/a" ++ blue "path.txt"
    , dirGreen "this is  " ++ blue "automatic formatting . txt"
    , concat [ "\""
             , dirGreen "ForMAtting / @ % ("
             , blue "consistency enforced ) ~ . & STYLES"
             , "\""]
    , intercalate "," [ blue "multiple.txt"
                      , green "folders"
                      , green "and"
                      , blue "files.txt" ]
    , blue ".dotFile"
    , dirGreen "dotted.path/a" ++ blue "b.txt"
    , "f,w,u,s,u," ++ green "choice of options"
    , "fileCreate,w,u,snakeCase,u,"
          ++ green "options can be written in full-form"
    , ",,,,," ++ green "default options"
    , green "also default options"
    , ",s,,,," ++ green "snake case with other options as default"
    , ",s,,,w," ++ intercalate "," [ dirGreen "options plus"
                                   , dirGreen "multiple"
                                   , blue "files.txt"
                                   , dirGreen "or folders" ]
    , "../" ++ dirGreen "parent directory" ++ blue "file.txt"
    , concat [ dirGreen "walking"
             , "../"
             , dirGreen "the"
             , duplicate 2 "../"
             , dirGreen "file system" ]
    , "/" ++ green "start at home directory"
    , concat [ "/"
             , dirGreen "combining"
             , "../"
             , blue "it.txt"
             , ","
             , dirGreen "all"
             , duplicate 3 "../"
             , " @(# "
             , dirGreen " @(# together%$ .@    "
             , concat [ ",s,camelCase,,,"
                      , dirGreen "for"
                      , ","
                      , dirGreen "our"
                      , blue ".amusement" ] ]
    , lineSurround $ heading "output" ----------
    , "The output of this command is color coded. e.g."
    , ""
    , concat [ dirRed "a"
             , dirRed "b"
             , dirGreen "c"
             , dirGreen "d"
             , "../"
             , dirRed "e"
             , blue "f.txt"
             , "\n" ++ indent
             , "/"
             , dirGreen "h"
             , dirGreen "i"
             , dirRed "j.txt"
             , skipMsg ]
    , lineSurround "The color code is:"
    , colorCode blue "blue" "for created files"
    , colorCode green "green" "for created directories"
    , colorCode (colored 22) "white" "for non-creation events"
    , colorCode red "red" "for errors and skipped creations"
    , lineSurround "For more help, open the readme in your browser:"
    , green readmeUrl ]
        where definition name explanation = concat $
                  [ duplicate 2 indent
                  , take col (blue name ++ repeat ' ')
                  , spaceSurround $ green ":"
                  , explanation ]
              define name explanation     = definition name (values explanation)
              description     = "Create one or more files and directory paths,\
                \ with automatic name formatting."
              heading s       = map toUpper s ++ ":"
              command         = (green "mkTouchPlus " ++)
              orUsage         = indent ++ "or"
              col             = 24
              hlHead ""       = ""
              hlHead (c:cs)   = blue [c] ++ cs
              hlFlag (c:cs)   = blue "-" ++ green ("-" ++ [c]) ++ blue cs
              tag             = surround "[" "]" . blue
              commas          = intercalate (green ",")
              slashes         = intercalate (green $ spaceSurround "/")
              settings        = constrFields . toConstr
                                              $ Settings "" "" "" "" "" [""]
              usage           = commas $ tag <$> settings
              flags           = tag (slashes $ hlFlag <$> flagsList)
              values x        = slashes $ hlHead <$> x
              flagsList       = [ "help", "version"]
              optionsList     = map fst
              ioOptions       = optionsList ioChoices
              sepOptions      = optionsList sepChoices
              caseOptions     = optionsList caseChoices
              extOptions      = optionsList extChoices ++ sepOptions
              sanOptions      = optionsList sanitiseChoices
              nameDefinition  = unlines
                  ["One or more names for files or directories that will be"
                  , replicate col ' '++ indent ++ "outputted. Continues the\
                      \ comma-separated list of arguments."]
              colorCode f s d = take 17 (f s ++ ":" ++ repeat ' ') ++ d
              -- TODO: take an amount dependent on whether f is used or else if it is just id

-- Composition ----------

select :: String -> p -> [(String, p)] -> p
select s otherwise [] = otherwise
select s otherwise (x:xs) = test s x
    where test s (a,f) = if eitherEq s (head' a) a
                            then f
                            else select s otherwise xs

sepChoices, caseChoices, extChoices, sanitiseChoices
          :: [(String, [String] -> [String])]
ioChoices :: [(String, String -> IO ())]

sepChoices = [ ("hyphenSep", hyphenSep)
             , ("snakeSep", snakeSep)
             , ("dotSep", dotSep)
             , ("whitespaceSep", whitespaceSep) ]

caseChoices = [ ("lowerCase", lowerCase)
              , ("upperCase", upperCase)
              , ("titleCase", titleCase)
              , ("camelCase", camelCase)
              , ("id",id) ]

extChoices = [ ("extSep",extSep) ]

sanitiseChoices = [ ("unix", unix)
                  , ("windows", windows)
                  , ("mac", mac)
                  , ("sensible", sensible)
                  , ("conservative", conservative) ]

ioChoices = [ ("fileCreate", fileCreate)
            , ("dirCreate", dirCreate)
            , ("smartCreate", smartCreate)
            , ("putStrLn", putStrLn) ]

sepChoice, caseChoice, extChoice, sanitiseChoice
         :: String -> [String] -> [String]
ioChoice :: String -> String -> IO ()

sepChoice separator         = select separator hyphenSep sepChoices
caseChoice characterCase    = select characterCase id caseChoices
extChoice extensionFormat   = select extensionFormat
                                  (sepChoice extensionFormat)
                                  extChoices
sanitiseChoice sanitisation = select sanitisation sensible sanitiseChoices
ioChoice ioOperation        = select ioOperation smartCreate ioChoices

mkTouchPlus :: Settings -> IO ()
mkTouchPlus (Settings { ioOperation
                      , separator
                      , characterCase
                      , extensionFormat
                      , sanitisation
                      , name }) = composition
    where composition = putStrLn $ show $ quadApply homeF pathF nameF extF <$> pathNameExt <$> name
          homeF = dropWhile isSpace
          pathF = \s -> noNulls $ tokenSepSanCase <$> splitWith "/" s
          nameF = tokenSepSanCase
          extF  = tokenApply $ extChoice extensionFormat
                             . san
          creator x = putLineSurround $ sequence_
              [ output (Output { home      = h
                               , path      = p
                               , name      = n
                               , extension = e
                               , ioOperation }) | (h,p,n,e) <- x ]
          tokenApply f    = concat . f <$> tokens
          tokenSepSanCase = tokenApply $ sepChoice separator
                                       . san
                                       . caseChoice characterCase
          san = noNulls . sanitiseChoice sanitisation

-- creator
--         $   quadApply homeF pathF nameF extF
--         <$> pathNameExt
--         <$> name
