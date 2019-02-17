import Control.Monad ((>=>))
import Data.Char (toLower, toUpper, isSpace)
import Data.Function (on)
import Data.List (intercalate, intersperse, groupBy)
import System.Directory (createDirectory)
import System.IO (appendFile)

-- Tokenisation ----------

groupStr :: Char -> String -> [String]
groupStr c s = let (start, end) = break (== c) s
               in start : if null end then [] else groupStr c (tail end)
-- TODO: use groupBy ((==) `on` isSpace) instead?

-- TODO: type signature for these?:
notNull = filter (not . null)
sections = notNull . groupStr '.'
tokens s = words <$> sections s

-- Separators ----------

hyphenSep, snakeSep, dotSep :: [String] -> [String]

hyphenSep = intersperse "-"
snakeSep = intersperse "_"
dotSep = intersperse "."

-- Cases ----------

lowerCase, upperCase, titleCase, camelCase :: String -> String

lowerCase = map toLower
upperCase = map toUpper
titleCase = groupBy ((==) `on` isSpace) >=> \(c:cs) -> toUpper c : cs
camelCase = groupBy ((==) `on` isSpace) >=> \(c:cs) -> c : titleCase cs
-- TODO: title and camel dont work yet
-- see here: https://stackoverflow.com/questions/34160196/haskell-capitalize-the-first-letter-of-each-word-in-a-string-without-losing-wh

-- IO ----------

-- TODO: print filename and 'success' etc after creating it

touch, mkDir :: String -> IO ()

touch s = appendFile s ""
mkDir s = createDirectory s

-- Composition ----------

-- maker :: (FilePath -> IO ()) -> ([String] -> [String]) -> String -> IO ()
-- maker io case' name = io $ dotSep $ case' <$> tokens name

-- maker'' io sep {- case' -} name = io $ (intercalate ".") $ (sep' sep) <$> tokens name
--     where eitherSep a b = (sep == a) || (sep == b)
--           sep' sep | eitherSep "h" "hyphenSep" = hyphenSep
--                    | eitherSep "s" "snakeSep"  = snakeSep
--                    | otherwise                 = hyphenSep

maker''' sep case' name = (case'' case') $ merge $ dot $ (sep' sep) <$> tokens name
    where eitherEq a b c = (a == b) || (a == c)
          eitherSep = eitherEq sep
          eitherCase = eitherEq case'
          sep' sep | eitherSep "h" "hyphenSep" = hyphenSep
                   | eitherSep "s" "snakeSep"  = snakeSep
                   | eitherSep "d" "dotSep"    = dotSep
                   | eitherSep "n" "noSep"     = id
                   | otherwise                 = hyphenSep
          dot = foldr (\x y -> if y == [] then x ++ y else x ++ ["."] ++ y) []
          merge = intercalate ""
          case'' case' | eitherCase "n" "noCase"    = id
                       | eitherCase "l" "lowerCase" = lowerCase
                       | eitherCase "u" "upperCase" = upperCase
                       | eitherCase "t" "titleCase" = titleCase
                       | eitherCase "c" "camelCase" = camelCase
                       | otherwise                  = id

t = maker''' "h" "t"



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
