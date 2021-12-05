{-# LANGUAGE TypeApplications #-}

module Day1 (sonarSweep1, sonarSweep2) where

import Utils (window, zipTailWith)

--
-- Part 1
--

-- | Count the number of times a depth measurement increases from the previous
-- measurement
sonarSweep1 :: String -> String
sonarSweep1 = show . countIncreases . fmap (read @Int) . lines

-- | Count the number of times an element is greater than the previous element
countIncreases :: [Int] -> Int
countIncreases = length . filter id . zipTailWith (<)

sonarSweep1Example :: String
sonarSweep1Example = unlines . fmap show $
    [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

--
-- Part 2
--

-- | Count the number of times the sum of measurements in a three-measurement
-- sliding window increases from the previous sum.
sonarSweep2 :: String -> String
sonarSweep2 =
    show .
    countIncreases . fmap sum . window 3 .
    fmap (read @Int) . lines


sonarSweep2Example :: String
sonarSweep2Example = unlines . fmap show $
    [607, 618, 618, 617, 647, 716, 769, 792]
