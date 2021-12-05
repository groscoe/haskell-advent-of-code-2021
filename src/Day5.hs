{-# LANGUAGE MultiWayIf #-}
module Day5 (hydrothermalVenture1, hydrothermalVenture2) where

import Data.Function ((&))
import Text.ParserCombinators.ReadP (ReadP, char, many1, string)
import Utils (parseNumber, runParser, token)
import Data.List (maximumBy, group, sort)
import Data.Ord (comparing)
import Data.Traversable (for)
import Control.Arrow ((&&&))
import Data.Maybe (fromJust)

--
-- Part 1
--

data Point = Point {getX :: Int, getY :: Int}
  deriving (Eq, Ord, Show)

data Segment = Segment {start :: Point, end :: Point}
  deriving (Eq, Ord, Show)

isHorizontal :: Segment -> Bool
isHorizontal seg = getY (start seg) == getY (end seg)

isVertical :: Segment -> Bool
isVertical seg = getX (start seg) == getX (end seg)

data Vent
    = Vertical Segment
    | Horizontal Segment
    | Diagonal Segment
  deriving Show

parseInput :: ReadP [Vent]
parseInput = many1 parseVent

-- | Parse a vent line as a line segment, ordering the start and end points
-- according to their x and y coordinates
parseVent :: ReadP Vent
parseVent = do
  firstPoint <- parsePoint
  token $ string "->"
  secondPoint <- parsePoint
  let (s, e) = if firstPoint < secondPoint
               then (firstPoint, secondPoint)
               else (secondPoint, firstPoint)
      seg = Segment s e
  if | isHorizontal seg -> pure $ Horizontal seg
     | isVertical seg -> pure $ Vertical seg
     | otherwise -> pure $ Diagonal seg


parsePoint :: ReadP Point
parsePoint = token $ Point <$> (parseNumber <* char ',') <*> parseNumber

-- | get all points in a given vent. Segments are only vertical, horizontal
-- or 45-degree diagonal lines
ventToPoints :: Vent -> [Point]
ventToPoints (Vertical s) = let x = getX (start s)
                                y0 = getY (start s)
                                y1 = getY (end s)
                             in Point x <$> [y0 .. y1]
ventToPoints (Horizontal s) = let y = getY (start s)
                                  x0 = getX (start s)
                                  x1 = getX (end s)
                               in (`Point` y) <$> [x0 .. x1]
ventToPoints (Diagonal s) = let x0 = getX (start s)
                                x1 = getX (end s)
                                y0 = getY (start s)
                                y1 = getY (end s)
                                -- the distance is the same horizontally as vertically in a 45-deg line
                                distance = x1 - x0
                                -- Since we order first on 'x', the diagonal can run upwards or downwards
                                nextY = if y1 > y0 then (+) else (-)
                             in (\n -> Point (x0 + n) (y0 `nextY` n)) <$> [0 .. distance]

-- | Count how many times each element appears in a list
count :: (Eq a, Ord a) => [a] -> [(a, Int)]
count = fmap (head &&& length) . group . sort

-- | number of points where at least two lines overlap
numCrossings :: [Vent] -> Int
numCrossings =
    length .
    filter (\(_, numTimes) -> numTimes > 1) .
    count .
    -- get all points with vents
    concatMap ventToPoints

-- | Consider only horizontal and vertical lines. At how many points do at least
-- two lines overlap?
hydrothermalVenture1 :: String -> String
hydrothermalVenture1 =
    show .
    numCrossings .
    -- consider only vertical and horizontal vent lines
    filter (not . isDiagonal) .
    fromJust . runParser parseInput
  where
    isDiagonal (Diagonal _) = True
    isDiagonal _ = False

--
-- Part 2
--

-- | Consider all of the lines. At how many points do at least two lines
-- overlap?
hydrothermalVenture2 :: String -> String
hydrothermalVenture2 = show . numCrossings . fromJust . runParser parseInput

-- Utils

hydrothermalVentureExample :: String
hydrothermalVentureExample =
  unlines
    [ "0,9 -> 5,9",
      "8,0 -> 0,8",
      "9,4 -> 3,4",
      "2,2 -> 2,1",
      "7,0 -> 7,4",
      "6,4 -> 2,0",
      "0,9 -> 2,9",
      "3,4 -> 1,4",
      "0,0 -> 8,8",
      "5,5 -> 8,2"
    ]
