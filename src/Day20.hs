module Day20 where

import Control.Arrow ((&&&))
import Utils (Cell, Matrix, applyRule, bin2dec, fromCells, mapMatrix, padWith, setCursor, toCells)
import Data.Char (digitToInt)

type Algorithm = String

--
-- Part 1
--

trenchMap1 :: String -> String
trenchMap1 =
  show
    . length
    . filter (== '#')
    . concat
    . uncurry (applyAlgorithmRepeatedly 2)
    . readInput

--
-- Part 2
--

trenchMap2 :: String -> String
trenchMap2 =
  show
  . length
  . filter (== '#')
  . concat
  . uncurry (applyAlgorithmRepeatedly 50)
  . readInput

applyAlgorithmRepeatedly :: Int -> String -> Matrix Char -> Matrix Char
applyAlgorithmRepeatedly 0 _ = id
applyAlgorithmRepeatedly n alg
  | odd n = applyAlgorithm '.' alg . applyAlgorithmRepeatedly (n-1) alg
  | otherwise = applyAlgorithm (head alg) alg . applyAlgorithmRepeatedly (n-1) alg

--
-- Utils
--

applyAlgorithm :: Char -> Algorithm -> Matrix Char -> Matrix Char
applyAlgorithm padding algorithm =
  fromCells
  . mapMatrix (getPixel algorithm)
  . toCells
  . padWith padding
  . padWith padding

getPixel :: Algorithm -> Cell Char -> Cell Char
getPixel algorithm cell =
  let index = bin2dec $ pixelToBit <$> concat cell
    in setCursor (algorithm !! index) cell

pixelToBit :: Char -> Int
pixelToBit c = if c == '.' then 0 else 1

readInput :: String -> (Algorithm, Matrix Char)
readInput =
  (head &&& tail . tail)
    . lines

trenchMapExample :: String
trenchMapExample =
  unlines
    [ "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#",
      "",
      "#..#.",
      "#....",
      "##..#",
      "..#..",
      "..###"
    ]
