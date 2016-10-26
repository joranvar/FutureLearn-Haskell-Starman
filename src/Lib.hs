-- | Starman - https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/116475
module Lib
  (
    -- * Domain types
    Secret(..)
  , Guess(..)
  -- * Exported functions
  , check
  ) where

data Secret = Secret { secret :: [(Char, Bool)] }
newtype Guess = Guess Char

check :: Secret -> Guess -> (Bool, Secret)
check s (Guess g) = (g `elem` (map fst $ secret s), Secret $ map (\(c, b) -> (c, b || c == g)) $ secret s)
