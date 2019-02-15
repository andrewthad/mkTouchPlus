
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

anyEq :: Eq a => [a] -> [a] -> Bool
anyEq x y = any id $ liftA2 (==) x y

help :: [[String]] -> IO ()
help x = mapM_ help' x
    where help' x = if ["-h", "--help"] `anyEq` x
                        then putStrLn "Help me!"
                        else return ()

maker' :: (FilePath -> IO ()) -> ([String] -> String) -> String -> IO ()
maker' io case' name = do
    let tokens' = tokens name
    if True
       then putStrLn "Help me!"
       else io $ dotCase $ case' <$> tokens name

tHyphen = maker touch hyphenCase
tTitle = maker touch titleCase
tCamel = maker touch camelCase
tSnake = maker touch snakeCase
tDot = maker touch dotCase
tUpper = maker touch upperCase
mHyphen = maker mkDir hyphenCase
mCamel = maker mkDir camelCase
t = maker' touch hyphenCase

-- TODO: change this combinatorial explosion of functions to a system that uses flags. Deal with the flags up there after you tokenise.
-- TODO: flags for formatting can be combined. e.g. snakeCase + upperCase


-- flags x = map (map (a `elem` ["-h", "--help"])) x

-- elem

-- anyEq :: Eq a => [a] -> [a] -> Bool
-- anyEq x y = any id $ liftA2 (==) x y

-- flags :: [[String]] -> IO ()
-- flags x = (anyEq ["-h", "--help"]) <$> x -- then putStrLn "It's the help!"
