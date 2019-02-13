import Text.ParserCombinators.Parsec
import Control.Monad (liftM2)

pair :: CharParser () (String, Maybe String)
pair = liftM2 (,) (many $ noneOf ".") (optionMaybe (char '.' >> many anyChar))

fn :: String -> Either ParseError (String, Maybe String)
fn input = parse pair "" input
