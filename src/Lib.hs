-- | Starman - https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/116475
module Lib
  (
    -- * Domain types
    Secret(..)
  , Public(..)
  , Guess(..)
  ) where

newtype Secret = Secret { secret :: String }
newtype Public = Public { public :: String }
newtype Guess = Guess Char
