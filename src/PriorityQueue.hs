module PriorityQueue
  ( PQueue,
    toList,
    fromList,
    insert,
    pop,
    top,
    null,
    map,
    update,
  )
where

import Data.List (sort)
import Prelude hiding (map, null)
import qualified Prelude as P
import qualified Data.Set as S

newtype PQueue a = PQueue { _toSet :: S.Set a}

null :: PQueue a -> Bool
null = S.null . _toSet

fromList :: Ord a => [a] -> PQueue a
fromList = PQueue . S.fromList

toList :: PQueue a -> [a]
toList = S.toList . _toSet

insert :: Ord a => a -> PQueue a -> PQueue a
insert x (PQueue xs) = PQueue (S.insert x xs)

pop :: Ord a => PQueue a -> (a, PQueue a)
pop (PQueue xs) =
  let (y, ys) = S.deleteFindMin xs
   in (y, PQueue ys)

top :: Ord a => PQueue a -> a
top = S.findMin . _toSet

map :: Ord b => (a -> b) -> PQueue a -> PQueue b
map f = PQueue . S.map f . _toSet

update :: Ord a => (a -> a) -> (a -> Bool) -> PQueue a -> PQueue a
update f p (PQueue xs) = PQueue $ S.foldr' apply xs xs
  where
    apply = \x ys -> if p x then S.insert (f x) (S.delete x ys) else ys