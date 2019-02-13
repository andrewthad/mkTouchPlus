import Text.ParserCombinators.Parsec
import Control.Monad (liftM2)

fnExt :: CharParser () (String, Maybe String)
fnExt = liftM2 (,) (many1 $ noneOf ".") (optionMaybe $ char '.' >> many anyChar)

fn :: String -> Either ParseError (String, Maybe String)
fn input = parse fnExt "" input
