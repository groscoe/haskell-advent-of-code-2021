module Graph where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)

-- | An adjacency list representation of a graph
newtype Graph a = Graph [(a, [a])]
  deriving (Show)

-- | Build a graph from a generalized matrix, using an updater function
buildGraph :: (Foldable t, Foldable f) => (Graph b -> a -> Graph b) -> t (f a) -> Graph b
buildGraph updateGraph = go emptyGraph
  where
    go = foldl' (foldl' updateGraph)

-- | Insert a vertex and its edges in a graph
insertVertex :: Eq a => a -> [a] -> Graph a -> Graph a
insertVertex v es (Graph g) = Graph $ (v, es) : g

-- | Get the neighbors of a given node.
neighbors :: Eq a => a -> Graph a -> [a]
neighbors v (Graph g) = fromMaybe [] (lookup v g)

-- | A graph with no edges
emptyGraph :: Graph a
emptyGraph = Graph []

-- | Traverse the graph in depth-first order, starting from a given vertex
dfs :: Eq a => [a] -> Graph a -> a -> [a]
dfs seen g v
  | v `elem` seen = seen
  | otherwise = foldl f (v : seen) (neighbors v g)
  where
    f seen n
      | n `elem` seen = seen
      | otherwise = dfs seen g n

-- | Get the connected components of a graph
components :: Eq a => Graph a -> [[a]]
components g@(Graph es) = go [] [] es
  where
    go acc _ [] = acc
    go acc seen ((v, _) : vs)
      | v `elem` seen = go acc seen vs
      | otherwise =
        let component = dfs [] g v
         in go (component : acc) (component <> seen) vs
