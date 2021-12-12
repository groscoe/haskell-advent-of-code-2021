module Day12 (passagePathing1, passagePathing2) where

import Data.Char (isLower)
import Graph (EdgeType (Undirected), Graph, allPaths', allSimplePaths, fromEdgeList, neighbors)
import Utils (splitOn)

--
-- Part 1
--

-- Our goal is to find the number of distinct paths that start at start, end at
-- end, and don't visit small caves more than once. There are two types of
-- caves: big caves (written in uppercase, like A) and small caves (written in
-- lowercase, like b). It would be a waste of time to visit any small cave more
-- than once, but big caves are large enough that it might be worth visiting
-- them multiple times. So, all paths you find should visit small caves at most
-- once, and can visit big caves any number of times.

-- How many paths through this cave system are there that visit small caves at most once?
-- passagePathing1 :: String -> String
passagePathing1 :: String -> String
passagePathing1 =
  show
    . length
    . (\g -> allPaths' g isSmallCave "end" [] "start")
    . parseInput

--
-- Part 2
--

-- Now, big caves can be visited any number of times, a single small cave can be
-- visited at most twice, and the remaining small caves can be visited at most
-- once. However, the caves named start and end can only be visited exactly once
-- each: once you leave the start cave, you may not return to it, and once you
-- reach the end cave, the path must end immediately. Given these new rules, how
-- many paths through this cave system are there?
passagePathing2 :: String -> String
passagePathing2 =
  show
    . length
    . (\g -> concatMap (allPaths'' g isSmallCave "end" ["start"]) (neighbors "start" g))
    . parseInput

allPaths'' :: Eq a => Graph a -> (a -> Bool) -> a -> [a] -> a -> [[a]]
allPaths'' g avoidRepeating end avoided curNode
  | curNode == end = [[end]]
  | curNode `elem` avoided = []
  | not (avoidRepeating curNode) = fmap (curNode :) (concatMap (allPaths'' g avoidRepeating end avoided) (neighbors curNode g))
  | otherwise =
    let ns = neighbors curNode g
        simplePaths = concatMap (allSimplePaths g end (curNode : avoided)) ns
        -- We want all paths where there are repetitions other than our current cave
        paths = concatMap (allPaths'' g avoidRepeating end (curNode : avoided)) ns
        -- We also want all paths where there are no other repetitions (the current cave will be repeated).
        paths' = concatMap (allPaths' g avoidRepeating end avoided) ns
     in -- As paths with no repetitions whatsoever will be in both lists, we need to remove them
        (curNode :) <$> filter (`notElem` simplePaths) (paths <> paths')

--
-- Utils
--

type Cave = String

parseInput :: String -> Graph Cave
parseInput =
  fromEdgeList Undirected
    . fmap (\line -> let [nodeA, nodeB] = splitOn '-' line in (nodeA, nodeB))
    . lines

isSmallCave :: Cave -> Bool
isSmallCave = all isLower

-- | 10 paths without repeating small caves, 36 paths repeating small caves at
-- most once.
passagePathingExampleSmall :: String
passagePathingExampleSmall =
  unlines
    [ "start-A",
      "start-b",
      "A-c",
      "A-b",
      "b-d",
      "A-end",
      "b-end"
    ]

-- 19 paths without repeating small caves, 103 paths repeating small caves at
-- most once
passagePathingExampleMedium :: String
passagePathingExampleMedium =
  unlines
    [ "dc-end",
      "HN-start",
      "start-kj",
      "dc-start",
      "dc-HN",
      "LN-dc",
      "HN-end",
      "kj-sa",
      "kj-HN",
      "kj-dc"
    ]

-- 226 paths without repeating small caves, 3509 paths repeating small caves at
-- most once
passagePathingExampleLarge :: String
passagePathingExampleLarge =
  unlines
    [ "fs-end",
      "he-DX",
      "fs-he",
      "start-DX",
      "pj-DX",
      "end-zg",
      "zg-sl",
      "zg-pj",
      "pj-he",
      "RW-he",
      "fs-DX",
      "pj-RW",
      "zg-RW",
      "start-pj",
      "he-WI",
      "zg-he",
      "pj-fs",
      "start-RW"
    ]
