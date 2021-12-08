module Day8 (sevenSegmentSearch1, sevenSegmentSearch2) where

import Utils (both, toZipper, fromZipper)
import Data.Bifunctor (second)
import Data.Maybe (isJust, fromJust, fromMaybe, listToMaybe)
import Data.List (permutations, sort, (\\))
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

-- The strategy here is to begin with all possibilities for each segment, and
-- then filter them as the samples come. We assume we always end with a complete
-- map

data SegmentMap = SegmentMap {
    a :: String,
    b :: String,
    c :: String,
    d :: String,
    e :: String,
    f :: String,
    g :: String
  } deriving Show

initialMap :: SegmentMap
initialMap = SegmentMap "abcdefg" "abcdefg" "abcdefg" "abcdefg" "abcdefg" "abcdefg" "abcdefg"

-- | For each sample received, update the segment possibilities
update :: SegmentMap -> String -> SegmentMap
update m one@[_, _] = -- only '1' has two segments (c and f)
    SegmentMap (a m \\ one) (b m \\ one) (only one (c m)) (d m \\ one) (e m \\ one) (only one (f m)) (g m \\ one)
update m seven@[_, _, _] = -- only '7' has three segments (a, c and f)
    SegmentMap (only seven (a m)) (b m \\ seven) (only seven (c m)) (d m \\ seven) (e m \\ seven) (only seven (f m)) (g m \\ seven)
update m four@[_, _, _, _] = -- only '4' has four segments (b, c, d and f)
    SegmentMap (a m \\ four) (only four (b m)) (only four (c m)) (only four (d m)) (e m \\ four) (only four (f m)) (g m \\ four)
-- '2', '3' and '5' have five segments. We reduce the possibilities only of the segments that are common to all of them
update m twoThreeFive@[_, _, _, _, _] =
    m { a = only twoThreeFive (a m), d = only twoThreeFive (d m), g = only twoThreeFive (g m) }
-- '0', '6' and '9' have six segments. We reduce the possibilities only of the segments that are common to all of them
update m zeroSixNine@[_, _, _, _, _, _] =
    m { a = only zeroSixNine (a m), b = only zeroSixNine (b m), f = only zeroSixNine (f m), g = only zeroSixNine (g m) }
update m _ = m

-- | a `only` b keeps only the elements of b that are also elements of a
only :: Eq a => [a] -> [a] -> [a]
only = filter . flip elem


-- | When we reach segments with only one possibility, remove this possibility
-- from the other segments
propagate :: SegmentMap -> SegmentMap
propagate = listToMap . fromZipper . go . toZipper . mapToList
  where
    go (before, [c], []) = (fmap (remove c) before, [c], [])
    go (before, cursor, []) = (before, cursor, [])
    go (before, [c], after : rest) = go ([c] : fmap (remove c) before, remove c after, fmap (remove c) rest)
    go (before, cursor, after : rest) = go  (cursor : before, after, rest)

    remove :: Char -> String -> String
    remove c = filter (/= c)


-- | For each entry, determine all of the wire/segment connections and decode
-- the four-digit output values. What do you get if you add up all of the output
-- values?
sevenSegmentSearch2 :: String -> String
sevenSegmentSearch2 = show . sum . fmap readingFromLine . lines

--
-- Utils
--
mapToList :: SegmentMap -> [String]
mapToList (SegmentMap a b c d e f g) = [a, b, c, d, e, f, g]

listToMap :: [String] -> SegmentMap
listToMap [a, b, c, d, e, f, g] = SegmentMap a b c d e f g
listToMap l = error $ "can't convert list: " <> show l

digitFromMap :: SegmentMap -> String -> Int
digitFromMap m@(SegmentMap a b c d e f g) str = flip fromMaybe (uniqueDigit str) $
  case sort $ foldl' getSelector [] str of
    "abcefg" -> 0
    "acdeg" -> 2
    "acdfg" -> 3
    "abdfg" -> 5
    "abdefg" -> 6
    "abcdfg" -> 9
    _ -> error $ "invalid digit '" <> str <> "' for map " <> show m
  where
    getSelector :: String -> Char -> String
    getSelector acc 'a' = a <> acc
    getSelector acc 'b' = b <> acc
    getSelector acc 'c' = c <> acc
    getSelector acc 'd' = d <> acc
    getSelector acc 'e' = e <> acc
    getSelector acc 'f' = f <> acc
    getSelector acc 'g' = g <> acc
    getSelector _ c = error $ "invalid segment: " <> show c

outputValue :: SegmentMap -> [String] -> Int
outputValue m = read . concatMap show . fmap (digitFromMap m)

invertMap :: SegmentMap -> SegmentMap
invertMap m@(SegmentMap a b c d e f g) = fromList . fmap snd $ sort [(a, "a"), (b, "b"), (c, "c"), (d, "d"), (e, "e"), (f, "f"), (g, "g")]
  where
    fromList [a, b, c, d, e, f, g] = SegmentMap a b c d e f g
    fromList cs = error $ "Can't build map from this list: " <> unwords cs

readingFromLine :: String -> Int
readingFromLine line =
  let (patterns, digits) = splitInput line
      segmentMap = mapFromPatterns patterns
   in outputValue (invertMap segmentMap) digits

mapFromPatterns :: [String] -> SegmentMap
mapFromPatterns = propagate . foldl' update initialMap

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
