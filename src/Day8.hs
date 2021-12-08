module Day8 where
import Utils (both)
import Data.Bifunctor (second)
import Data.Maybe (isJust)

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
