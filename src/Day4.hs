{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Day4 (giantSquid1) where
import Data.List (transpose, find, mapAccumR)
import Text.ParserCombinators.ReadP (ReadP, sepBy1, char)
import Data.Functor (void)
import Utils (parseNumber, runParser, splitOn)
import Data.Maybe (listToMaybe)

import Debug.Trace

--
-- part 1
--

 -- A 5x5 row-major matrix of integers and a flag showing if they were marked or not
type Board = [[(Int, Bool)]]

giantSquid1 :: String -> String
giantSquid1 = show . uncurry boardScore . uncurry drawUntilVictory . parseInput

-- | The score of the winning is the sum of all unmarked numbers multiplies by
-- the last number called
boardScore :: Int -> Board -> Int
boardScore lastNo = (*lastNo) . sum . fmap (sum . fmap fst . filter (not . snd))

-- | Keep drawing numbers until a board winds. Return the last drawn number paired with the board
drawUntilVictory :: [Int] -> [Board] -> (Int, Board)
drawUntilVictory ns bs = go (draw bs ns)
  where
    go ((n, bs) : next) = case find hasWon bs of
      Nothing -> go next
      Just board -> (n, board)
    go _ = error "No winner after drawing all numbers"

draw :: [Board] -> [Int] -> [(Int, [Board])]
draw bs [] = []
draw bs (n:ns) = let marked = markNumber n <$> bs in (n, marked) : draw marked ns
  where
    markNumber :: Int -> Board -> Board
    markNumber n = fmap (fmap (\m -> if fst m == n then (n, True) else m))

-- | A winning board has all the numbers in a row or a column marked
hasWon :: Board -> Bool
hasWon b = any completed (rows b) || any completed (columns b)
  where
    rows = id
    columns = transpose
    completed = all snd

-- | Read a line of drawn numbers followed by boards separated by newlines
parseInput :: String -> ([Int], [Board])
parseInput s =
  let ([ns] : bs) = splitOn "" (lines s)
   in (read @Int <$> splitOn ',' ns, readBoard <$> bs)

readBoard :: [String] -> Board
readBoard = fmap (fmap (mark . read @Int) . words)
  where
    mark = (, False)

-- Utilities

-- Just to help with debugging
printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

showBoard :: Board -> String
showBoard = unlines . fmap (unwords . fmap withMark)
  where
    -- Show marked numbers in bold
    withMark (n, True) = "\ESC[1m" <> show n <> "\ESC[0m"
    withMark (n, False) = show n

giantSquidExample :: String
giantSquidExample = unlines
  [
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
    "",
    "22 13 17 11  0",
    "8  2 23  4 24",
    "21  9 14 16  7",
    "6 10  3 18  5",
    "1 12 20 15 19",
    "",
    "3 15  0  2 22",
    "9 18 13 17  5",
    "19  8  7 25 23",
    "20 11 10 24  4",
    "14 21 16 12  6",
    "",
    "14 21 17 24  4",
    "10 16 15  9 19",
    "18  8 23 26 20",
    "22 11 13  6  5",
    "2  0 12  3  7"
  ]
