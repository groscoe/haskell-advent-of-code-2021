{-# LANGUAGE BangPatterns #-}

module Utils where

import Data.Bifunctor (first, bimap)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.Functor (void, ($>))
import Data.List (transpose)
import Text.ParserCombinators.ReadP (ReadP, char, get, look, munch1, readP_to_S, skipSpaces)

-- * List utilities

-- | Zip a list with its own tail
zipTail :: [a] -> [(a, a)]
zipTail = zipTailWith (,)

-- | Zip a list with its own tail using the function in the first argument
zipTailWith :: (a -> a -> b) -> [a] -> [b]
zipTailWith f xs = zipWith f xs (tail xs)

-- | Slice a list into windows of a fixed size. The list must be larger than the
-- window size.
window :: Int -> [a] -> [[a]]
window windowSize xs
  | windowSize <= 1 = fmap (: []) xs
  | otherwise = zipWith (:) xs $ window (windowSize - 1) (tail xs)

-- | Split a list on each occurrence of a separator
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = go []
  where
    go acc (c : cs)
      | c == sep = reverse acc : go [] cs
      | otherwise = go (c : acc) cs
    go acc [] = [reverse acc]

-- | Split a list in half in O(n) using the 'torsoise and hare' algorithm
halve :: [a] -> ([a], [a])
halve xs = go xs xs
  where
    go (x : xs) (_ : _ : ys) =
      let (xs', ys') = go xs ys
       in (x : xs', ys')
    go xs ys = ([], xs)

-- | Insert an element at the beginning and at the end of a list
surroundWith :: a -> [a] -> [a]
surroundWith x xs = x : xs <> [x]

--
-- Matrices
--

type Matrix a = [[a]]

indexMatrix :: Matrix a -> Matrix (Int, Int, a)
indexMatrix = indexMatrixWith id

indexMatrixWith :: ((Int, Int, a) -> b) -> Matrix a -> Matrix b
indexMatrixWith f = fmap indexCell . zip [0 ..] . fmap (zip [0 ..])
  where
    indexCell (row, xs) = fmap (\(col, value) -> f (row, col, value)) xs

showMatrix :: Show a => [[a]] -> String
showMatrix = unlines . fmap (unwords . fmap show)

mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = fmap (fmap f)

-- | Pad the sides of a matrix with a given value
padWith :: a -> Matrix a -> Matrix a
padWith x [] = []
padWith x ys@(firstLine : _) =
  let n = length firstLine + 2
      boundaries = replicate n x
   in surroundWith boundaries $ fmap (surroundWith x) ys

-- | Concatenate two matrices side-by-side
beside :: [[a]] -> [[a]] -> [[a]]
beside (xs : xss) (ys : yss) = xs <> ys : beside xss yss
beside [] yss = yss
beside xss [] = xss

-- | Concatenate two matrices one above the other
below :: [[a]] -> [[a]] -> [[a]]
below xss yss = xss <> yss

--
-- Cells (3x3 matrices for 2D automata)
--

type Cell a = [[a]]

toCells :: Matrix a -> [[Cell a]]
toCells = fmap (fmap transpose . window 3 . transpose) . window 3

fromCells :: [[Cell a]] -> Matrix a
fromCells = fmap (fmap getCursor)

getCursor :: Cell a -> a
getCursor xs = let [_, [_, x, _], _] = xs in x

type Rule a = Cell a -> Cell a

applyRule :: a -> Rule a -> Matrix a -> Matrix a
applyRule padding r = fromCells . mapMatrix r . toCells . padWith padding

-- * Zipper utilities

type Zipper a = ([a], a, [a])

toZipper :: [a] -> Zipper a
toZipper (x : xs) = ([], x, xs)
toZipper [] = error "Empty list cannot be converted to zipper"

fromZipper :: Zipper a -> [a]
fromZipper (xs, cursor, ys) = reverse (cursor : xs) <> ys

-- * Parsing

-- | Try to parse a string until the end
runParser :: ReadP a -> String -> Maybe a
runParser p input = case [x | (x, "") <- readP_to_S p input] of
  [result] -> Just result
  _ -> Nothing

-- | parse a sequence of digits as a number
parseNumber :: (Read a, Integral a) => ReadP a
parseNumber = do
  firstDigit <- head <$> look
  sign <- case firstDigit of
    '-' -> get $> negate
    _ -> pure id
  digits <- token $ munch1 isDigit
  pure . sign $ read digits

-- | parses a newline character, followed by zero or more spaces. Currently accepts only '\n'
newline :: ReadP ()
newline = void . token $ char '\n'

-- | Parse a symbol possibly followed by whitespace
token :: ReadP a -> ReadP a
token p = p <* skipSpaces

-- * Frequency counters

type Counter a =
  -- | A simple frequency dictionary
  [(a, Integer)]

-- | Update the count in a given key by a function, inserting with a default
-- value if necessary
updateCounter :: Eq a => (Integer -> Integer) -> Integer -> a -> Counter a -> Counter a
updateCounter f e k [] = [(k, e)]
updateCounter f e k (item@(k', !v) : rest)
  | k == k' = (k, f v) : rest
  | otherwise = item : updateCounter f e k rest

mapCounterKeys :: (a -> b) -> Counter a -> Counter b
mapCounterKeys f = fmap (first f)

sumCounter :: Counter a -> Integer
sumCounter = sum . fmap snd

-- * General utilities

-- | Apply a function to both arguments of an homogeneous tuple
both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

-- | Get the 3rd element of tuple
trd :: (a, b, c) -> c
trd (_, _, c) = c

-- | Convert a little-endian list of bits to an integer
bin2dec :: [Int] -> Int
bin2dec = foldl' (\n b -> b + 2 * n) 0 -- a.k.a <https://en.wikipedia.org/wiki/Horner%27s_method Horner's method>

-- | Compute minimum and maximum values of a (non-empty, finite) list
bounds :: Ord a => [a] -> (a, a)
bounds = boundsBy id

boundsBy :: Ord b => (a -> b) -> [a] -> (a, a)
boundsBy _ [] = error "Utils.bounds: empty list"
boundsBy f (x : xs) = foldl' (minMaxBy f) (x, x) xs
  where
    minMaxBy :: Ord b => (a -> b) -> (a, a) -> a -> (a, a)
    minMaxBy f (!mi, !ma) y = (minBy f y mi, maxBy f y ma)

    minBy f x y = if f x < f y then x else y
    maxBy f x y = if f y < f x then x else y

-- | Floating-point infinity
inf :: Fractional a => a
inf = 1 / 0

-- | Iterate until no changes are detected
untilEqual :: Eq a => (a -> a) -> a -> a
untilEqual f x =
  let x' = f x
   in if x == x' then x else untilEqual f x'

iterateM' :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM' 0 _ x = pure [x]
iterateM' i f x = f x >>= iterateM' (i -1) f

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
