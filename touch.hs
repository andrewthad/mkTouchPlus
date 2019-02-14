import Data.List (intercalate)
-- import Turtle

main :: IO ()
main = do return ()

groupBy :: String -> Char -> [String]
groupBy s c = let (start, end) = break (== c) s
              in start : if null end then [] else groupBy (tail end) c

notNull = filter (not . null)
split c s = notNull $ groupBy s c
sections = split '.'
terms = split ' '
tokens s = terms <$> sections s
hyphenate = intercalate "-"
hyphenated s = hyphenate <$> tokens s
dotted l = intercalate "." l
fn = dotted . hyphenated
