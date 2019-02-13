import Text.ParserCombinators.Parsec
import Control.Monad (liftM2)

-- main = do return ()
--
-- csvFile = endBy line eol
-- line = sepBy cell (char ',')
-- cell = many (noneOf ",\n")
-- eol = char '\n'
--
-- parseCSV :: String -> Either ParseError [[String]]
-- parseCSV input = parse csvFile "(unknown)" input

-- touchString s = sepEndBy s (char '.')
-- touchString  = "test, test, test" `sepBy` (char ',')
    -- do
    -- filename <- 
    -- extension <-
    -- return (filename, extension)

-- parseTouch :: String -> (String, String)
-- parseTouch = parse touchString "(unknown)"

import Text.ParserCombinators.Parsec

-- csvFile = endBy letters eol
-- letters = many $ noneOf "\n"
-- eol = char '\n'
--
-- parseCSV :: String -> Either ParseError [[Char]]
-- parseCSV input = parse csvFile "(unknown)" input

-- csvFile = sepBy letters comma
-- letters = many $ noneOf ","
-- comma = char ','
--
-- commas :: String -> Either ParseError [String]
-- commas input = parse csvFile "(unknown)" input

-- p_pair_app1 =
--     liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

-- p_pair :: CharParser () (String, Maybe String)
-- p_pair = do
--   name <- many1 p_char
--   value <- optionMaybe (char '=' >> many p_char)
--   return (name, value)

-- pair :: CharParser () (String, Maybe String)
-- pair = do
--     name <- notDot
--     ext <- optionMaybe (char '.' >> notDot)
--     return (name, ext)

notDot = many $ noneOf "."

pair :: CharParser () (String, Maybe String)
pair = liftM2 (,) notDot (optionMaybe (char '.' >> notDot))

fn :: String -> Either ParseError (String, Maybe String)
fn input = parse pair "(unknown)" input
