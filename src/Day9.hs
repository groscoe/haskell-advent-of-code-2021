module Day9 (smokeBasin1, smokeBasin2) where

import Data.List (foldl', sortBy, transpose)
import Graph (Graph, components, emptyGraph, insertVertex, buildGraph)
import Utils (inf, surroundWith, window, trd, indexMatrix)

--
-- Smoke basin
--

-- The strategy is simple: we split the input into 3x3 overlapping cells and
-- look at the center element (the 'cursor') of each one to know if it's a
-- minimum

-- 3x3 matrix
type Cell a = [[a]]

-- | Find all of the low points on your heightmap. What is the sum of the risk
-- levels of all low points on your heightmap?
smokeBasin1 :: String -> String
smokeBasin1 = show . round . sum . fmap riskLevel . lowPoints . splitInput . parseLines
  where
    riskLevel :: Double -> Double
    riskLevel = succ

    lowPoints :: [[Cell Double]] -> [Double]
    lowPoints = concatMap (fmap cellCursor . filter isMinimum)

    isMinimum :: Cell Double -> Bool
    isMinimum [[_, above, _], [before, cursor, after], [_, below, _]] = all (cursor <) [above, before, after, below]
    isMinimum cell = error $ "isMinimum: invalid cell: " <> show cell

    cellCursor :: Cell Double -> Double
    cellCursor [_, [_, x, _], _] = x
    cellCursor cell = error $ "cellCursor: invalid cell: " <> show cell



--
-- Part2
--

-- If we represent the depth map as a graph, where adjacent squares are
-- connections and every node with a depth of '9' is disconnected from the rest,
-- the problem reduces to finding the connected components of this graph.

-- | What do you get if you multiply together the sizes of the three largest
-- basins?
smokeBasin2 :: String -> String
smokeBasin2 =
  show
    . product
    . take 3
    . sortBy (flip compare)
    . fmap length
    -- connected components of the cell graph ("basins")
    . components
    . buildGraph insertCellCursor
    -- 3x3 cells with an indexed cursor in the middle
    . toCells
    -- We index the nodes so they can be uniquely compared in the graph
    . indexMatrix
    . addBoundaries
    . parseLines
  where
    addBoundaries xs =
      let n = length (head xs)
          boundaries = replicate n inf
       in surroundWith boundaries xs

    -- | Given a cell, insert its central element along with its adjacent elements in the graph
    insertCellCursor :: Graph (Int, Int, Double) -> Cell (Int, Int, Double) -> Graph (Int, Int, Double)
    insertCellCursor g [[_, above, _], [before, cursor, after], [_, below, _]]
      | trd cursor == 9 = g
      | otherwise =
        let connections = filter (\(_, _, x) -> x < 9) [above, before, after, below]
          in insertVertex cursor connections g
    insertCellCursor _ _ = error "invalid input"

--
-- Utils
--

-- | Split lines of digits into 9-value cells
splitInput :: [[Double]] -> [[Cell Double]]
splitInput numbers =
  let n = length (head numbers)
      boundary = replicate n inf
   in toCells $ surroundWith boundary numbers

toCells :: [[a]] -> [[Cell a]]
toCells = fmap (fmap transpose . window 3 . transpose) . window 3

parseLines :: String -> [[Double]]
parseLines = fmap (surroundWith inf . fmap (read . (: []))) . lines

type LineBasin = (Int, Int, Int, Int)


smokeBasinExample :: String
smokeBasinExample =
  unlines
    [ "2199943210",
      "3987894921",
      "9856789892",
      "8767896789",
      "9899965678"
    ]
