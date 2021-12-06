{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Day6 (lanternfish1, lanternfish2) where

import Control.Arrow (first)
import Data.Foldable (Foldable (foldl'))
import Utils (Counter, mapCounterKeys, splitOn, sumCounter, updateCounter)

--
-- Part 1
--

-- | Read a list of comma-separated ints, each representing a fish and its day
-- in the cycle, into a frequency counter
readInput :: String -> Counter Int
readInput =
  foldl' incrementCounter initialCounter
    . fmap (read @Int)
    . splitOn ','
  where
    initialCounter :: Counter Int
    initialCounter = [(i, 0) | i <- [0 .. 8]]

    incrementCounter :: Counter Int -> Int -> Counter Int
    incrementCounter c i = updateCounter (+ 1) 1 i c

-- | Simulate the fish population through n days of the reproduction cycle
simulate :: Int -> Counter Int -> Counter Int
simulate = go
  where
    go 0 counter = counter
    go !numDays ((0, !numFish) : countGreaterThanZero) =
      -- in every iteration, decrement the counter and reset the counter of the fish that reproduced
      let decrementedCounter = updateCounter (+ numFish) numFish 6 $ mapCounterKeys pred countGreaterThanZero
          -- and put the new fishes at the 8th day
          newCounter = updateCounter (+ numFish) numFish 8 decrementedCounter
       in go (numDays - 1) newCounter
    go _ _ = error "day 0 not in head of the counter"

-- | How many lanternfish would there be after 80 days?
lanternfish1 :: String -> String
lanternfish1 = show . sumCounter . simulate 80 . readInput

--
-- Part 2
--

-- | How many lanternfish would there be after 256 days?
lanternfish2 :: String -> String
lanternfish2 = show . sumCounter . simulate 256 . readInput

-- utilities

lanternfishExample :: String
lanternfishExample = "3,4,3,1,2"