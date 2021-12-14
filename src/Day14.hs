module Day14 where
import Text.ParserCombinators.ReadP
import Data.Char (isUpper, isSpace, isLetter)
import Utils (token, runParser, boundsBy, bounds, updateCounter, Counter, iterateM')
import Data.Maybe (fromJust)
import Data.List (sort, group)
import Data.Foldable (foldl')
import State (State(State, runState), modify)

--
-- part1
--

-- | Apply 10 steps of pair insertion to the polymer template and find the most
-- and least common elements in the result. What do you get if you take the
-- quantity of the most common element and subtract the quantity of the least
-- common element?
extendedPolymerization1 :: String -> String
extendedPolymerization1 =
  show
  . uncurry subtract
  . bounds
  . fmap snd
  . snd
  . uncurry (runInsertions 10)
  . fromJust
  . runParser parseInput

runInsertions :: Int -> String -> [InsertionRule] -> (Counter String, Counter Char)
runInsertions numSteps initial rules =
  runState (head <$> iterateM' numSteps (step rules) (toPairCounter initial))
  $ toCounter initial

-- | Run a single pair insertion step, accumulating the counts of both the pairs
-- produced and each letter in the final string
step :: [InsertionRule] -> Counter String -> State (Counter Char) (Counter String)
step rules (([x, y], n) : rest) = do
  let newLetter = fromJust (lookup [x, y] rules)
  modify (updateCounter (+n) n newLetter)
  restCounter <- step rules rest
  pure $ updateCounter (+n) n [x,newLetter] . updateCounter (+n) n [newLetter, y] $ restCounter
step _ _ = pure []

--
-- Part 2
--

-- | Apply 40 steps of pair insertion to the polymer template and find the most
-- and least common elements in the result. What do you get if you take the
-- quantity of the most common element and subtract the quantity of the least
-- common element?
extendedPolymerization2 :: String -> String
extendedPolymerization2 =
  show
  . uncurry subtract
  . bounds
  . fmap snd
  . snd
  . uncurry (runInsertions 40)
  . fromJust
  . runParser parseInput

--
-- Utils
--

type InsertionRule = (String, Char)

parseInput :: ReadP (String, [InsertionRule])
parseInput = (,) <$> token (munch1 isLetter) <*> many1 parseInsertionRule

parseInsertionRule :: ReadP InsertionRule
parseInsertionRule = token $ (,) <$> munch1 isUpper <*> (string " -> " *> satisfy isLetter)

toCounter :: String -> Counter Char
toCounter = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (updateCounter succ 1 x acc) xs

toPairCounter :: String -> Counter String
toPairCounter = go []
  where
    go acc (x:y:xs) = go (updateCounter succ 1 [x, y] acc) (y:xs)
    go acc _ = acc

extendedPolymerizationExample :: String
extendedPolymerizationExample =
    unlines [
        "NNCB",
        "",
        "CH -> B",
        "HH -> N",
        "CB -> H",
        "NH -> C",
        "HB -> C",
        "HC -> B",
        "HN -> C",
        "NN -> C",
        "BH -> H",
        "NC -> B",
        "NB -> B",
        "BN -> B",
        "BB -> N",
        "BC -> B",
        "CC -> N",
        "CN -> C"
    ]
