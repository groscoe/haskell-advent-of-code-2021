{-# LANGUAGE BangPatterns #-}

module Utils where

import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.Functor (void)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, readP_to_S, skipSpaces)

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

-- * Parsing

-- | Try to parse a string until the end
runParser :: ReadP a -> String -> Maybe a
runParser p input = case [x | (x, "") <- readP_to_S p input] of
  [result] -> Just result
  _ -> Nothing

-- | parse a sequence of digits as a number
parseNumber :: (Read a, Integral a) => ReadP a
parseNumber = do
  digits <- token $ munch1 isDigit
  pure $ read digits

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

-- | Convert a little-endian list of bits to an integer
bin2dec :: [Int] -> Int
bin2dec = foldl' (\n b -> b + 2 * n) 0 -- a.k.a <https://en.wikipedia.org/wiki/Horner%27s_method Horner's method>
