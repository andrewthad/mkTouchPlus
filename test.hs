{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data

data Rec = Rec {
    alpha :: Int,
    beta  :: Double,
    phi   :: Float 
}  deriving (Data, Typeable)

sample = Rec 1 2.3 4.5

main :: IO ()
main = print . constrFields . toConstr $ sample 
