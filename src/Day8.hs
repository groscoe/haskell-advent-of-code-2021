{-# LANGUAGE FlexibleInstances #-}
module Day8 (sevenSegmentSearch1, sevenSegmentSearch2) where
import Utils (both)
import Data.Bifunctor (second)
import Data.Maybe (isJust, fromJust)
import Data.Functor.Identity (Identity (..))
import Data.List (permutations, sort)
import Data.Foldable (foldl')

{- 7-segment display mapping
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
-}

--
-- Part 1
--

-- | Because the digits 1, 4, 7, and 8 each use a unique number of segments, you
-- should be able to tell which combinations of signals correspond to those
-- digits. In the output values, how many times do digits 1, 4, 7, or 8 appear?
sevenSegmentSearch1 :: String -> String
sevenSegmentSearch1 =
  show . sum . fmap (countUniqueDigits . snd . splitInput) . lines
  where
    countUniqueDigits :: [String] -> Int
    countUniqueDigits = length . filter (isJust . uniqueDigit)

-- | Check if a given output digit has a single unique match, based on the
-- number of segments (i.e., it is 1, 7, 4 or 8)
uniqueDigit :: String -> Maybe Int
uniqueDigit [_, _] = Just 1 -- only '1' has two segments
uniqueDigit [_, _, _] = Just 7 -- only '7' has three segments
uniqueDigit [_, _, _, _] = Just 4   -- only '4' has four segments
uniqueDigit [_, _, _, _, _, _, _] = Just 8 -- only '8' has seven segments
uniqueDigit _ = Nothing

-- | Split an input line into (signal patterns, output digits)
splitInput :: String -> ([String], [String])
splitInput = both words . second tail . span (/= '|')


--
-- Part 2
--

data IncompleteMap = SegmentMap {
  a :: Maybe Char,
  b :: Maybe Char,
  c :: Maybe Char,
  d :: Maybe Char,
  e :: Maybe Char,
  f :: Maybe Char,
  g :: Maybe Char
} deriving Show

data CompleteMap = CompleteMap {
  a' :: Char,
  b' :: Char,
  c' :: Char,
  d' :: Char,
  e' :: Char,
  f' :: Char,
  g' :: Char
} deriving Show

emptyMap :: IncompleteMap
emptyMap = SegmentMap Nothing Nothing Nothing Nothing Nothing Nothing Nothing

isComplete :: IncompleteMap -> Maybe CompleteMap
isComplete (SegmentMap (Just a) (Just b) (Just c) (Just d) (Just e) (Just f) (Just g)) = Just $ CompleteMap a b c d e f g
isComplete _ = Nothing

getPossibilities :: String -> [IncompleteMap]
getPossibilities one@[_, _] = flip fmap (permutations one) $
  \[c, f] -> emptyMap { c = Just c, f = Just f}
getPossibilities seven@[_, _, _] = flip fmap (permutations seven) $
  \[a, c, f] -> emptyMap {a = Just a, c = Just c, f = Just f}
getPossibilities four@[_, _, _, _] = flip fmap (permutations four) $
  \[b, c, d, f] -> emptyMap {b = Just b, c = Just c, d = Just d, f = Just f}
getPossibilities eight@[_, _, _, _, _, _, _] = flip fmap (permutations eight) $
  \[a, b, c, d, e, f, g] -> SegmentMap (Just a) (Just b) (Just c) (Just d) (Just e) (Just f) (Just g)
getPossibilities twoThreeOrFive@[_, _, _, _, _] = flip concatMap (permutations twoThreeOrFive) $
  \[u, v, x, y, z] -> [
    -- Either a '2'
    emptyMap {a = Just u, c = Just v, d = Just x, e = Just y, g = Just z},
    -- Or a '3'
    emptyMap {a = Just u, c = Just v, d = Just x, f = Just y, g = Just z},
    -- Or a 5
    emptyMap {a = Just u, b = Just v, d = Just x, f = Just y, g = Just z}
  ]
getPossibilities zeroSixOrNine@[_, _, _, _, _, _] = flip concatMap (permutations zeroSixOrNine) $
  \[u, v, w, x, y, z] -> [
    -- Either a '0'
    emptyMap {a = Just u, b = Just v, c = Just w, e = Just x, f = Just y, g = Just z},
    -- Or a '6'
    emptyMap {a = Just u, b = Just v, d = Just w, e = Just x, f = Just y, g = Just z},
    -- Or a '9'
    emptyMap {a = Just u, b = Just v, c = Just w, d = Just x, f = Just y, g = Just z}
  ]
getPossibilities err = error $ "Undecodable pattern: " <> err

merge :: IncompleteMap -> IncompleteMap -> Maybe IncompleteMap
merge mapA mapB = case traverse ((\f -> f mapA mapB) . mergeDigitOn) [a, b, c, d, e, f, g] of
  Nothing -> Nothing
  Just [a', b', c', d', e', f', g'] -> Just $ SegmentMap a' b' c' d' e' f' g'
  _ -> error "impossible"
  where
    mergeDigitOn f m1 m2 = mergeDigit (f m1) (f m2)
    mergeDigit (Just d1) (Just d2)
      | d1 == d2 = Just (Just d1)
      | otherwise = Nothing
    mergeDigit (Just d1) _ = Just (Just d1)
    mergeDigit _ d2 = Just d2

mergeMultiple :: [IncompleteMap] -> [IncompleteMap] -> [IncompleteMap]
mergeMultiple m1s m2s = fromJust <$> filter isJust [merge m1 m2 | m1 <- m1s, m2 <- m2s]

mapFromPatterns :: [String] -> Maybe CompleteMap
mapFromPatterns patterns = case foldl1 mergeMultiple . fmap getPossibilities . filter ((/= Just 8) . uniqueDigit) $ patterns of
  [finalMap] -> isComplete finalMap
  _ -> Nothing


digitFromMap :: CompleteMap -> String -> Int
digitFromMap m@(CompleteMap a b c d e f g) str = case uniqueDigit str of
  Just n -> n
  Nothing -> case sort $ foldl' getSelector [] str of
    "abcefg" -> 0
    "acdeg" -> 2
    "acdfg" -> 3
    "abdfg" -> 5
    "abdefg" -> 6
    "abcdfg" -> 9
    _ -> error $ "invalid digit '" <> str <> "' for map " <> show m
  where
    getSelector :: String -> Char -> String
    getSelector acc 'a' = a : acc
    getSelector acc 'b' = b : acc
    getSelector acc 'c' = c : acc
    getSelector acc 'd' = d : acc
    getSelector acc 'e' = e : acc
    getSelector acc 'f' = f : acc
    getSelector acc 'g' = g : acc
    getSelector _ c = error $ "invalid segment: " <> show c

outputValue :: CompleteMap -> [String] -> Int
outputValue m = read . concatMap show . fmap (digitFromMap m)

-- invertMap :: CompleteMap -> CompleteMap
invertMap :: CompleteMap -> CompleteMap
invertMap m@(CompleteMap a b c d e f g) = fromList . fmap snd $ sort [(a, 'a'), (b, 'b'), (c, 'c'), (d, 'd'), (e, 'e'), (f, 'f'), (g, 'g')]
  where
    fromList [a, b, c, d, e, f, g] = CompleteMap a b c d e f g
    fromList cs = error $ "Can't build map from this list: " <> cs

readingFromLine :: String -> Int
readingFromLine line =
  let (patterns, digits) = splitInput line
   in case mapFromPatterns patterns of
     Just segmentMap -> outputValue (invertMap segmentMap) digits
     _ -> error $ "invalid line: " <> line


-- | For each entry, determine all of the wire/segment connections and decode
-- the four-digit output values. What do you get if you add up all of the output
-- values?
sevenSegmentSearch2 :: String -> String
sevenSegmentSearch2 = show . sum . fmap readingFromLine . lines

--
-- Utils
--
sevenSegmentSearchExample :: String
sevenSegmentSearchExample = unlines [
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
        "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
        "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
        "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
        "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
        "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
        "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
        "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
        "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
        "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    ]
