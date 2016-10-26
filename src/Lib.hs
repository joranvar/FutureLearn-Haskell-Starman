-- | Starman - https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/116475
module Lib
  (
    -- * Domain types
    Secret(..)
  , Public(..)
  , Guess(..)
  -- * Exported functions
  , check
  ) where

newtype Secret = Secret { secret :: String }
newtype Public = Public { public :: String }
newtype Guess = Guess Char

check :: Secret -> Public -> Guess -> (Bool, Public)
check s p (Guess g) = (g `elem` secret s, p)
