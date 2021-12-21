module Day21 (diracDice1) where

import Control.Arrow ((&&&))
import Data.List (scanl')
import Utils (both)

{-
Let n be the current turn played by the players and S(n) be the position of player
1 at the n-th turn. Then:

  S(0) = 4
  S(n) = mod(S(n-1) + 6*(3*n-2) -1, 10) + 1

If we expand the relation, we note a cycle (spaces added manually):

λ> show $ fmap s [1..20]
"[  10,4,6,6,4,  10,4,6,6,4,  10,4,6,6,4,  10,4,6,6,4  ]"

of period 5 and sum 30. That is, at every 5 turns, player 1's score is increased by 30.

So, at the n-th turn, player 1 will have:

P1(n) | n `mod` 5 == 0 = 4 + (n `div` 5) * 30 + 6
      | n `mod` 5 == 1 = 4 + (n `div` 5) * 30 + 6 + 4
      | n `mod` 5 == 2 = 4 + (n `div` 5) * 30 + 6 + 4 + 6
      | n `mod` 5 == 3 = 4 + (n `div` 5) * 30 + 6 + 4 + 6 + 6
      | n `mod` 5 == 4 = 4 + (n `div` 5) * 30 + 6 + 4 + 6 + 6 + 4

 Similarly, let S2(n) be the position of player 2 in the n-th turn. Then:

 S2(0) = 8
 S2(n) = mod(S(n-1) + 3*(6*n-1) -1, 10) + 1

 An expansion shows the cycle [3,6,7,6,3,8,1,2,1,8], of period 10 and sum 45.

 Therefore, player 1 wins since 1000/30*5 < 1000/45 * 10.
-}

--

-- * Part1

--

diracDice1 :: String -> String
diracDice1 =
  show
    . computeScore 0
    . takeWhilePlus1 (\(x, y) -> x < 1000 && y < 1000)
    . tail
    . uncurry zip
    . both accumulatingSum
    . unzip
    . (\(m, n) -> (s1 m &&& s2 n) <$> [1 ..])
    . readInput
  where
    accumulatingSum = scanl' (+) 0

    computeScore nRolls [(_', p2'), (p1, p2)]
      | p1 > p2 = (nRolls + 9) * p2'
      | otherwise = (nRolls + 12) * p2
    computeScore nRolls (_ : xs) = computeScore (nRolls + 6) xs
    computeScore _ _ = error "impossible"

s1 :: Int -> Int -> Int
s1 initial 0 = initial
s1 initial n = (s1 initial (n -1) + 6 * (3 * n -2) - 1) `mod` 10 + 1

s2 :: Int -> Int -> Int
s2 initial 0 = initial
s2 initial n = (s2 initial (n -1) + 3 * (6 * n - 1) - 1) `mod` 10 + 1

--

-- * Utils

--
readInput :: String -> (Int, Int)
readInput =
  (\[x, y] -> (x, y))
    . fmap (read . last . words)
    . lines

takeWhilePlus1 :: (a -> Bool) -> [a] -> [a]
takeWhilePlus1 p [] = []
takeWhilePlus1 p (x : xs)
  | not (p x) = [x]
  | otherwise = x : takeWhilePlus1 p xs
