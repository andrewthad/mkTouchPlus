import Data.Char (toUpper)
import Data.List (intersperse)
import System.Directory (createDirectory)
import System.IO (appendFile)

-- Utilities ----------

groupBy :: String -> Char -> [String]
groupBy s c = let (start, end) = break (== c) s
              in start : if null end then [] else groupBy (tail end) c

eitherEq :: (Eq a) => a -> a -> a -> Bool
eitherEq a b c = (a == b) || (a == c)

-- Tokenisation ----------

notNull = filter (not . null)
split c s = notNull $ groupBy s c
sections = split '.'
terms = split ' '
tokens s = terms <$> sections s

-- Separators ----------

hyphenSep :: [String] -> [String]
hyphenSep = intersperse "-"

snakeSep :: [String] -> [String]
snakeSep = intersperse "_"

dotSep :: [String] -> [String]
dotSep = intersperse "."

titleSep :: [String] -> String
titleSep [] = ""
titleSep [x] = x
titleSep (x:xs) = title x ++ titleSep xs
    where title (c:cs) = toUpper c : cs

camelSep :: [String] -> String
camelSep [] = []
camelSep [x] = x
camelSep (x:xs) = x ++ titleSep xs

-- Cases ----------

-- IO ----------

-- TODO: print filename and 'success' etc after creating it

touch :: FilePath -> IO ()
touch name = appendFile name ""

mkDir :: FilePath -> IO ()
mkDir name = createDirectory name

-- Composition ----------

-- maker :: (FilePath -> IO ()) -> ([String] -> [String]) -> String -> IO ()
-- maker io case' name = io $ dotSep $ case' <$> tokens name

maker'' io sep {- case' -} name = io $ dotSep $ (sep' sep) <$> tokens name
    where sep' sep | eitherEq sep "h" "hyphenSep" = hyphenSep
                   | eitherEq sep "s" "snakeSep"  = snakeSep
                   | eitherEq sep "n" "noSep"     = id
                   | otherwise                    = hyphenSep

-- TODO: use monads? (>>=) :: Monad m => m a -> (a -> m b) -> m b

t sep {- case' -} = maker'' touch sep

-- sep: none, hyphen, underscore
-- case: lower, upper, title, camel

-- tHyphen = maker touch hyphenCase
-- tTitle = maker touch titleCase
-- tCamel = maker touch camelCase
-- tSnake = maker touch snakeCase
-- tDot = maker touch dotCase
-- tUpper = maker touch upperCase
-- mHyphen = maker mkDir hyphenCase
-- mCamel = maker mkDir camelCase
