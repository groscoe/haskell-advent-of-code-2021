module Utils where

import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Text.ParserCombinators.ReadP (ReadP, munch1, readP_to_S)

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

-- | Try to parse a string until the end
runParser :: ReadP a -> String -> Maybe a
runParser p input = case [x | (x, "") <- readP_to_S p input] of
  [result] -> Just result
  _ -> Nothing

-- | parse a sequence of digits as a number
parseNumber :: (Read a, Integral a) => ReadP a
parseNumber = do
  digits <- munch1 isDigit
  pure $ read digits

-- | Apply a function to both arguments of an homogeneous tuple
both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

-- | Convert a little-endian list of bits to an integer
bin2dec :: [Int] -> Int
bin2dec = foldl' (\n b -> b + 2 * n) 0 -- a.k.a <https://en.wikipedia.org/wiki/Horner%27s_method Horner's method>

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = go []
  where
    go acc (c : cs)
      | c == sep = reverse acc : go [] cs
      | otherwise = go (c:acc) cs
    go acc [] = [reverse acc]