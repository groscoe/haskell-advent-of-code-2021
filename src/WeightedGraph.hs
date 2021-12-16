{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

module WeightedGraph
  ( Graph,
    toMap,
    emptyGraph,
    vertices,
    neighbors,
    toAdjacencies,
    fromAdjacencies,
    insertEdge,
    shortestPath,
  )
where

import Data.Bifunctor (second)
import Data.Foldable (Foldable (foldl'))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified PriorityQueue as PQ
import Utils (inf)

newtype Graph w a = Graph {toMap :: M.Map a (M.Map a w)}

data EdgeType = Directed | Undirected
  deriving (Eq, Show)

emptyGraph :: Ord a => Graph w a
emptyGraph = Graph mempty

vertices :: Graph w a -> [a]
vertices = M.keys . toMap

neighbors :: Ord a => a -> Graph w a -> [(a, w)]
neighbors v = M.toList . fromMaybe mempty . M.lookup v . toMap

toAdjacencies :: Graph w a -> [(a, [(a, w)])]
toAdjacencies = fmap (second M.toList) . M.toList . toMap

fromAdjacencies :: Ord a => [(a, a, w)] -> Graph w a
fromAdjacencies = foldl' insertEdge emptyGraph

insertEdge :: Ord a => Graph w a -> (a, a, w) -> Graph w a
insertEdge g (!n1, !n2, !w)
  | n1 == n2 = g
  | otherwise =
    let vs = neighbors n1 g
     in Graph $ M.insert n1 (M.fromList $ (n2, w) : vs) (toMap g)

data WithInfinity n = Number {unsafeFromNumber :: !n} | Infinity
  deriving (Ord, Eq, Show, Functor)

-- | The shortest path (with distance from start node) from the start to the end node
shortestPath :: Graph Int (Int, Int) -> (Int, Int) -> (Int, Int) -> [((Int, Int), Int)]
shortestPath g start end =
  let queue = PQ.fromList . fmap (\x -> if x == start then (x, 0) else (x, maxBound)) $ vertices g
   in go [] queue
  where
    go :: [((Int, Int), Int)] -> PQ.PQueue ((Int, Int), Int) -> [((Int, Int), Int)]
    go acc unvisited
      | PQ.null unvisited = acc
      | fst (PQ.top unvisited) == end = PQ.top unvisited : acc
      | otherwise =
        let ((!x, !distance), otherNodes) = PQ.pop unvisited -- distance from the start node
            adjacents = neighbors x g
            rest = foldl' (updateDistance distance) otherNodes adjacents
         in go ((x, distance) : acc) rest

    updateDistance :: Int -> PQ.PQueue ((Int, Int), Int) -> ((Int, Int), Int) -> PQ.PQueue ((Int, Int), Int)
    updateDistance distance q (!node, !weight) =
      PQ.update (\(_, !originalDistance) -> (node, min originalDistance (weight + distance))) ((== node) . fst) q
    {-# INLINE updateDistance #-}
