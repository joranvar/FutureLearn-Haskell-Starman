{-# LANGUAGE TupleSections #-}

-- | Starman - https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/116475
module Lib
  (
    -- * Domain types
    Secret(..)
  , Guess(..)
  -- * Exported functions
  , check
  , turn
  , starman
  ) where

data Secret = Secret { secret :: [(Char, Bool)] }
newtype Guess = Guess Char

check :: Secret -> Guess -> (Bool, Secret)
check s (Guess g) = (g `elem` (map fst $ secret s), Secret $ map (\(c, b) -> (c, b || c == g)) $ secret s)

turn :: Secret -> Int -> IO ()
turn _ 0                      = putStrLn "You lose"
turn (Secret s) _ | all snd s = putStrLn "You win!"
turn s n                      = mkguess s n

mkguess :: Secret -> Int -> IO ()
mkguess s n = do
  putStrLn (public s ++ " " ++ replicate n '*')
  putStr "  Enter your guess: "
  q <- getLine
  let (correct, s') = check s (Guess $ q!!0)
  let n' = if correct then n else n-1
  turn s' n'

public :: Secret -> String
public = map (\(c, b) -> if b then c else '-') . secret

starman :: String -> Int -> IO ()
starman s n = turn (Secret $ map (,False) s) n
