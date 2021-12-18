{-# LANGUAGE TupleSections #-}

module Day17 (trickShot1, trickShot2) where

import Data.Bifunctor (second)
import Control.Monad (void)
import Data.List (nub)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP (ReadP, string)
import Utils (parseNumber, runParser)

--
-- Part 1
--

{-
  As the y-trajectory describes a (discrete) parabola, the highest y-velocity we
  can use at the start is the one that, when the probe reaches (y=0) again,
  takes a single step to get to the bottom of the target. As the velocity
  increases by 1 at a time, this desired velocity is one plus the y-position of
  the bottom of the target.
-}

-- | Find the initial velocity that causes the probe to reach the highest y
-- position and still eventually be within the target area after any step. What
-- is the highest y position it reaches on this trajectory?
trickShot1 :: String -> String
trickShot1 =
  show
    . (\(minX, maxX, minY, maxY) -> last . takeWhileIncreasing $ y (abs $ minY + 1) <$> [1 ..])
    . fromJust
    . runParser parseInput
  where
    takeWhileIncreasing (x : y : ys)
      | x < y = x : takeWhileIncreasing (y : ys)
      | otherwise = [x]
    takeWhileIncreasing xs = xs

{-
We have:

    x(0) = 0
    x(n+1) = x(n) + max(v_x - n, 0)

    y(0) = 0
    y(n+1) = y(n) + (v_y - n)
-}

-- That means:

x :: Int -> Int -> Int
x v n
  | v == 0 = 0
  | v > n = floor $ -1 / 2 * fromIntegral (v * (-2 * v + v - 1) - (v - n) * (2 * n - 2 * v + (v - n) - 1))
  | otherwise = floor $ -1 / 2 * fromIntegral (v * (-2 * v + v - 1))

y :: Int -> Int -> Int
y v n = floor $ -1 / 2 * fromIntegral (n * (n - 2 * v - 1))

--
-- Part 2
--

{-
  We see that:
    - The x-position stops increasing when N=v_x
    - At that step, we can calculate X (sum from 0 to N = n*(n + 1) / 2)
    - We don't need any v_x that goes to zero before xMin
    - Therefore, we don't need any v_x such that v_x*(v_x + 1) / 2 < xMin
      - => i.e. any such that (abs v_x) < (sqrt (8*xMin + 1) - 1) / 2
      - That is, the first v_x worth considering is ceiling ((sqrt (8*xMin + 1) - 1) / 2)
    - At the same time, the largest v_x worth considering is xMax (any more will overshoot).

  Also:
    - v_y decreases by 1 at each step
    - We don't need to consider v_y lower then yMin (any more will overshoot).
    - For positive y, the largest v_y worth considering is such that -v_y won't
      overshoot (see part 1), i.e., yMin + 1
    - For a given v_y and y, n = (sqrt (4*v_y^2 + 4*v_ y - 8*y + 1) + 2*v_y + 1) / 2
    - For each y from yMin to yMax, whenever n is integer the probe hits the target.
-}

-- | How many distinct initial velocity values cause the probe to be within the
-- target area after any step?
trickShot2 :: String -> String
trickShot2 =
  show
    . (\(xMin, xMax, yMin, yMax) -> length $ getValidVelocities xMin xMax yMin yMax)
    . fromJust
    . runParser parseInput

getValidVelocities :: Int -> Int -> Int -> Int -> [(Int, Int)]
getValidVelocities xMin xMax yMin yMax =
  nub
    . concatMap distributeAndFlip
    . fmap (second getVxFromNs)
    $ getNsFromYs yMin yMax
  where
    vXMin = ceiling $ (sqrt (8 * fromIntegral xMin + 1) - 1) / 2
    vXMax = xMax

    getVxFromNs = concatMap getVxFromN

    getVxFromN n = filter (hitsXTarget xMin xMax n) [vXMin .. vXMax]

    distributeAndFlip (y, xs) = fmap (,y) xs

getNsFromYs :: Int -> Int -> [(Int, [Int])]
getNsFromYs yMin yMax =
  (\v -> (v, fmap floor . filter isInteger $ (\y -> (sqrt (fromIntegral $ 4 * v ^ 2 + 4 * v - 8 * y + 1) + 2 * fromIntegral v + 1) / 2) <$> [yMin .. yMax]))
    <$> [yMin .. abs (yMin + 1)]
  where
    isInteger x = fromInteger (floor x) == x

hitsXTarget :: Int -> Int -> Int -> Int -> Bool
hitsXTarget xMin xMax n vX =
  let x' = x vX n
   in xMin <= x' && x' <= xMax

--
-- Utils
--

parseInput :: ReadP (Int, Int, Int, Int)
parseInput = do
  void $ string "target area: x="
  (minX, maxX) <- (,) <$> (parseNumber <* string "..") <*> parseNumber
  void $ string ", y="
  (minY, maxY) <- (,) <$> (parseNumber <* string "..") <*> parseNumber
  pure (minX, maxX, minY, maxY)

trickShotExample :: String
trickShotExample = "target area: x=20..30, y=-10..-5"