{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
module Graph where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Bifunctor (Bifunctor(second, bimap))

-- | An adjacency list representation of a graph
newtype Graph a = Graph { toAscList :: [(a, [a])] }
  deriving (Show)

instance Functor Graph where
  fmap f = Graph . fmap (bimap f (fmap f)) . toAscList

class Container (f :: * -> *) where
  isElem :: Eq a => a -> f a -> Bool

  isNotElem :: Eq a => a -> f a -> Bool
  isNotElem x = not . isElem x

instance Container [] where
  isElem = elem
  isNotElem = notElem

instance Container Graph where
  isElem x = isElem x . vertices

vertices :: Graph a -> [a]
vertices = fmap fst . toAscList

edges :: Graph a -> [(a, a)]
edges = concatMap (\(x, xs) -> (x,) <$> xs) . toAscList

mapNeighbors :: (a -> a) -> Graph a -> Graph a
mapNeighbors f = Graph . fmap (second $ fmap f) . toAscList

updateNode :: Eq a => a -> [a] -> Graph a -> Graph a
updateNode v es g
  | v `isElem` g = let Graph vs = g in insertVertex v es (Graph $ filter (\(u, _) -> u /= v) vs)
  | otherwise = insertVertex v es g

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
dfs = dfs' (:)

-- | Accumulate a graph in depth-first order, starting from a given vertex
dfs' :: (Container f, Eq a) => (a -> f a -> f a) -> f a -> Graph a -> a -> f a
dfs' f seen g v
  | v `isElem` seen = seen
  | otherwise = foldl (walkNeighbors f) (f v seen) (neighbors v g)
  where
    walkNeighbors f seen n
      | n `isElem` seen = seen
      | otherwise = dfs' f seen g n

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
