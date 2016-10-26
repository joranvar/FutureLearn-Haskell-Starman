{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

import Lib
instance (Monad m, Enum a, Bounded a) => Serial m a where
  series = generate (\d -> take d [minBound .. maxBound])

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" scTests
  , testGroup "Unit tests" huTests
  ]

scTests :: [TestTree]
scTests =
  [ testProperty "Guessing a letter that exists, yields (True, _)" $
    \secret' guess -> (fst . check (Secret { secret = (guess, False) : secret'})) (Guess guess) == True
  , testProperty "Guessing a letter that exists, sets all occurrences of that letter to True" $
    \secret' guess ->
      let (Secret s) = (snd . check (Secret { secret = (guess, False) : secret'})) (Guess guess)
          found = filter ((== guess) . fst) s
      in (all id $ map snd found) && (not . null) found
  ]

huTests :: [TestTree]
huTests =
  [ testCase "True is True" $
    True @?= True
  ]
