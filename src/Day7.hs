module Day7 (treacheryOfWhales1, treacheryOfWhales2) where

import Utils (bounds, splitOn)

--
-- Part 1
--

-- | Determine the horizontal position that the crabs can align to using the
-- least fuel possible. How much fuel must they spend to align to that position?
treacheryOfWhales1 :: String -> String
treacheryOfWhales1 = show . leastFuelCost simpleMovementCost . readInput
  where
    simpleMovementCost to from = abs (to - from)

-- | Compute the least amount of fuel required to move every crab to the same
-- horizontal position
leastFuelCost ::
  -- | Fuel consumption to get to the (1) new position from the (2) old one
  (Int -> Int -> Int) ->
  -- | Initial crab positions
  [Int] ->
  Int
leastFuelCost move ns = minimum $ fmap fuelCost [lower .. upper]
  where
    (lower, upper) = bounds ns

    fuelCost :: Int -> Int
    fuelCost newPosition = sum $ fmap (move newPosition) ns

--
-- Part 2
--

-- As it turns out, crab submarine engines don't burn fuel at a constant rate.
-- Instead, each change of 1 step in horizontal position costs 1 more unit of
-- fuel than the last: the first step costs 1, the second step costs 2, the
-- third step costs 3, and so on.
proportionalFuelCost :: Int -> Int -> Int
proportionalFuelCost to from = let n = abs (from - to) in n * (n + 1) `div` 2

-- | Determine the horizontal position that the crabs can align to using the
-- least fuel possible so they can make you an escape route! How much fuel must
-- they spend to align to that position?
treacheryOfWhales2 :: String -> String
treacheryOfWhales2 = show . leastFuelCost proportionalFuelCost . readInput

--
-- Utils
--
readInput :: String -> [Int]
readInput = fmap read . splitOn ','

treacheryOfWhalesExample :: String
treacheryOfWhalesExample = "16,1,2,0,4,2,7,1,2,14"