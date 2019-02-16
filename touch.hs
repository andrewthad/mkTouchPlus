
import Control.Applicative (liftA2)
import Data.Char (toUpper)
import Data.List (intercalate)
import System.Directory (createDirectory)
import System.IO (appendFile)

-- Tokenisation ----------

groupBy :: String -> Char -> [String]
groupBy s c = let (start, end) = break (== c) s
              in start : if null end then [] else groupBy (tail end) c

notNull = filter (not . null)
split c s = notNull $ groupBy s c
sections = split '.'
terms = split ' '
tokens s = terms <$> sections s

-- Cases ----------

hyphenCase :: [String] -> String
hyphenCase = intercalate "-"

snakeCase :: [String] -> String
snakeCase = intercalate "_"

dotCase :: [String] -> String
dotCase = intercalate "."

titleCase :: [String] -> String
titleCase [] = ""
titleCase [x] = x
titleCase (x:xs) = title x ++ titleCase xs
    where title (c:cs) = toUpper c : cs

camelCase :: [String] -> String
camelCase [] = []
camelCase [x] = x
camelCase (x:xs) = x ++ titleCase xs

upperCase :: [String] -> String
upperCase [] = []
upperCase (x:xs) = (map toUpper x) ++ upperCase xs

-- IO ----------

-- TODO: print filename and 'success' etc after creating it

touch :: FilePath -> IO ()
touch name = appendFile name ""

mkDir :: FilePath -> IO ()
mkDir name = createDirectory name

-- Composition ----------

maker :: (FilePath -> IO ()) -> ([String] -> String) -> String -> IO ()
maker io case' name = io $ dotCase $ case' <$> tokens name

maker' :: (FilePath -> IO ()) -> ([String] -> String) -> String -> IO ()
maker' io case' name = do
    let tokens' = tokens name
    if True
       then putStrLn "Help me!"
       else io $ dotCase $ case' <$> tokens name

maker'' 

tHyphen = maker touch hyphenCase
tTitle = maker touch titleCase
tCamel = maker touch camelCase
tSnake = maker touch snakeCase
tDot = maker touch dotCase
tUpper = maker touch upperCase
mHyphen = maker mkDir hyphenCase
mCamel = maker mkDir camelCase
t = maker' touch hyphenCase

-- TODO: use maker as the only function. use two arguments for case: sep and case
-- sep: none, hyphen, underscore
-- case: lower, upper, title, camel
