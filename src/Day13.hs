module Day13 (transparentOriginal1, transparentOriginal2) where

import Text.ParserCombinators.ReadP (ReadP, char, string, many, (<++))
import Utils (parseNumber, runParser)
import Data.List (nub)
import Data.Maybe (fromJust)
import Control.Monad (forM)

--
-- Part 1
--

-- How many dots are visible after completing just the first fold instruction on
-- your transparent paper?
transparentOriginal1 :: String -> String
transparentOriginal1 =
  show
  . length
  . (\(cs, is) -> applyInstructions cs [head is])
  . fromJust
  . runParser parseInput

applyInstructions :: [Coords] -> [FoldInstruction] -> [Coords]
applyInstructions cs is = foldr ((.) . foldCoords) id (reverse is) cs

--
-- Part 2
--

-- Finish folding the transparent paper according to the instructions. The
-- manual says the code is always eight capital letters.

transparentOriginal2 :: String -> String
transparentOriginal2 =
  draw
  . uncurry applyInstructions
  . fromJust
  . runParser parseInput

--
-- Utils
--

type Coords = (Int, Int)

foldCoords :: FoldInstruction -> [Coords] -> [Coords]
foldCoords (FoldUp y) = nub . reflectUp y
foldCoords (FoldLeft x) = nub . reflectLeft x

reflectUp :: Int -> [Coords] -> [Coords]
reflectUp y = nub . fmap (\p@(x', y') -> if y' > y then (x', y' - 2*(y' - y)) else p)

reflectLeft :: Int -> [Coords] -> [Coords]
reflectLeft x = nub . fmap (\p@(x', y') -> if x' > x then (x' - 2*(x' - x), y') else p)

data FoldInstruction
  = FoldUp Int
  | FoldLeft Int
  deriving (Show)

parseInput :: ReadP ([Coords], [FoldInstruction])
parseInput = (,) <$> many parseCoords <*> many (parseFoldLeft <++ parseFoldUp)

parseCoords :: ReadP (Int, Int)
parseCoords = (,) <$> parseNumber <*> (char ',' *> parseNumber)

parseFoldLeft :: ReadP FoldInstruction
parseFoldLeft = FoldLeft <$> (string "fold along x=" *> parseNumber)

parseFoldUp :: ReadP FoldInstruction
parseFoldUp = FoldUp <$> (string "fold along y=" *> parseNumber)

transparentOriginalExample :: String
transparentOriginalExample =
  unlines
    [ "6,10",
      "0,14",
      "9,10",
      "0,3",
      "10,4",
      "4,11",
      "6,0",
      "6,12",
      "4,1",
      "0,13",
      "10,12",
      "3,4",
      "3,0",
      "8,4",
      "1,10",
      "2,14",
      "8,10",
      "9,0",
      "",
      "fold along y=7",
      "fold along x=5"
    ]


draw' :: [Coords] -> [[String]]
draw' cs = do
  let maxY = maximum (snd <$> cs)
      maxX = maximum (fst <$> cs)
  forM [0..maxY] $ \y -> do
    let coordsInLine = filter ((== y) . snd) cs
    line <- forM [0..maxX] $ \x ->
      case lookup x coordsInLine of
        Nothing -> " "
        Just y' -> if y' == y then "#" else " "
    pure $ '\n' : line

draw :: [Coords] -> String
draw = concat . concat . draw'