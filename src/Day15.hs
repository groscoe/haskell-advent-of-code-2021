module Day15 where

import Data.Char (digitToInt)
import Utils ( indexMatrix, fst3, snd3, beside, below )
import Data.Foldable (foldl')
import WeightedGraph (insertEdge, Graph (toMap), emptyGraph, shortestPath, vertices)
import Data.Maybe (catMaybes, fromMaybe)

import Algorithm.Search ( aStar, pruning )
import qualified Data.Map.Strict as M

--
-- Part 1
--

chiton1 :: String -> String
chiton1 =
  show
  . snd
  . head
  . pathOfLeastRisk
  . fst
  . toGraph
  . parseInput

--
-- Part 2
--

chiton2 :: String -> String
chiton2 s =
  let m = expandSquare 5 . parseInput $ s
      (g, (maxX, maxY)) = toGraph m
      target = maximum (vertices g)
   in
     show
     . maybe 0 fst
     . shortestWithAStar maxX maxY g (0, 0)
     $ target

-- This time, my hand-rolled graph ADT didn't cut it and I had to use a library.

shortestWithAStar :: Int -> Int -> Graph Int (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, [(Int, Int)])
shortestWithAStar maxX maxY g start end =
  aStar (ns `pruning` isWall) weight (dist end) (== end) start
  where
    dist (x1, y1) (x2, y2) = abs (y2 -y1) + abs (x2 -x1)

    weight cur next = (M.! next) . fromMaybe mempty . M.lookup cur . toMap $ g

    isWall (x, y) = x < 0 || x > maxX || y < 0 || y > maxY

    ns :: (Int, Int) -> [(Int, Int)]
    ns (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

--
-- Utils
--

parseInput :: String -> [[Int]]
parseInput =
  fmap (fmap digitToInt)
  . lines

toGraph :: [[Int]] -> (Graph Int (Int, Int), (Int, Int))
toGraph m =
  let indexedPairs = concat (indexMatrix m)
      maxX = maximum $ fmap fst3 indexedPairs
      maxY = maximum $ fmap snd3 indexedPairs
   in (addEdgesToAdjacentNodes maxX maxY emptyGraph indexedPairs, (maxX, maxY))
  where
    addEdgesToAdjacentNodes :: Int -> Int -> Graph Int (Int, Int) -> [(Int, Int, Int)] -> Graph Int (Int, Int)
    addEdgesToAdjacentNodes _ _ g [] = g
    addEdgesToAdjacentNodes maxX maxY g ((x, y, w) : rest) = addEdgesToAdjacentNodes maxX maxY (foldl' insertEdge g (adjacentNodes x y w)) rest
      where
        -- Add an edge from nodes above, below, and to the sides to the current
        -- node. The weight of the edge is the number in the node
        adjacentNodes :: Int -> Int -> Int -> [((Int, Int), (Int, Int), Int)]
        adjacentNodes x y w =
          let p = (x, y) in catMaybes [
            if x == 0 then Nothing else Just ((x-1, y), p, w),
            if x == maxX then Nothing else Just ((x+1, y), p, w),
            if y == 0 then Nothing else Just ((x, y-1), p, w),
            if y == maxY then Nothing else Just ((x, y+1), p, w)
          ]

expandSquare :: Int -> [[Int]] -> [[Int]]
expandSquare n xss = expandRows n (expandColumns n xss)
  where
    expandColumns 0 _ = []
    expandColumns n xss = xss `beside` expandColumns (n-1) (fmap incrementWrap <$> xss)

    expandRows 0 _ = []
    expandRows n xss = xss `below` expandRows (n-1) (fmap incrementWrap <$> xss)

    incrementWrap n = if n == 9 then 1 else n + 1

pathOfLeastRisk :: Graph Int (Int, Int) -> [((Int, Int), Int)]
pathOfLeastRisk g = shortestPath g (0,0) (maximum $ vertices g)

chitonExample :: String
chitonExample =
  unlines
    [ "1163751742",
      "1381373672",
      "2136511328",
      "3694931569",
      "7463417111",
      "1319128137",
      "1359912421",
      "3125421639",
      "1293138521",
      "2311944581"
    ]
