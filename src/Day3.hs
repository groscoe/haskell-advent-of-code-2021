module Day3 (binaryDiagnostic1, binaryDiagnostic2) where

import Data.Foldable (Foldable (foldl'))
import Utils (both, bin2dec)
import Data.Maybe (fromJust)
import qualified Data.Char as C
import Control.Arrow ((&&&))

--
-- Part 1
--

-- A trivial algorithm is enough here: we accumulate the frequencies of ones and
-- zeros, compute the desired rates according to each rule and multiply them

type Rates = (Int, Int) -- gamma and epsilon rate, respectively

binaryDiagnostic1 :: String -> String
binaryDiagnostic1 =
  show
    . multiplyRates
    . ratesFromBitFrequencies
    . bitFrequenciesInLines
    . lines
  where
    multiplyRates :: Rates -> Int
    multiplyRates (gamma, epsilon) = gamma * epsilon


-- | Compute the gamma and epsilon rates from the bit frequencies
ratesFromBitFrequencies :: [(Int, Int)] -- ^ Counts of zeros and ones
  -> Rates
ratesFromBitFrequencies = both bin2dec . unzip . fmap toBits
  where
    -- | Convert pairs of frequencies to pairs of (most common bit, least common bit)
    toBits :: (Int, Int) -> (Int, Int)
    toBits (zeros, ones) = if zeros > ones then (0, 1) else (1, 0)


-- | Count the most common and least common bits in each position of the report
bitFrequenciesInLines :: [String] -- ^ A list of bit strings
  -> [(Int, Int)] -- ^ counts of zeros and ones
bitFrequenciesInLines [] = []
bitFrequenciesInLines report@(firstLine : _) = bitFrequencies (length firstLine) report


-- | Count the number of ones and zeros in each position for a container of
-- bit-strings
bitFrequencies :: Foldable t => Int -> t String -> [(Int, Int)]
bitFrequencies numBits = foldl' updateFrequencies (initializeFrequencies numBits)
  where
    updateFrequencies :: [(Int, Int)] -> String -> [(Int, Int)]
    updateFrequencies = zipWith updatePosition

    updatePosition :: (Int, Int) -> Char -> (Int, Int)
    updatePosition (zeros, ones) bit = case bit of
      '0' -> (zeros + 1, ones)
      '1' -> (zeros, ones + 1)
      _ -> error $ "invalid bit: " <> show bit

    initializeFrequencies :: Int -> [(Int, Int)]
    initializeFrequencies n = replicate n (0, 0)

binaryDiagnostic1Example :: String
binaryDiagnostic1Example =
  unlines
    [ "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010"
    ]

--
-- Part 2
--

-- An easy way to solve this challenge is to read the bitstrings into a "binary
-- trie", a simple binary tree with values on leaves where the position of a
-- give bitstring is computed from its structure: for each bit, branch left if
-- it's zero; else branch right. With this trie in hand, we can find each rating
-- by branching according to its rule.

-- | A binary tree with items stored at the leaves. The number of leaves is
-- stored at each branch as an optimization
data BinTree a
  = Empty
  | Leaf a
  | Branch Int (BinTree a) (BinTree a)
  deriving Show

-- | Insert a bitstring at a leaf after traversing the tree according to its
-- bits: if the current bit is '0', branch left; else, branch right.
insert :: String -> BinTree String -> BinTree String
insert item = go item item
  where
    go item "" Empty = Leaf item
    go item ('0':bs) Empty = Branch 1 (go item bs Empty) Empty
    go item ('1':bs) Empty = Branch 1 Empty (go item bs Empty)
    go item ('0':bs) (Branch s l r) = Branch (s + 1) (go item bs l) r
    go item ('1':bs) (Branch s l r) = Branch (s + 1) l (go item bs r)
    go item _ _ = error "malformed input"

-- | Build up a tree from a list of bitstrings
makeTree :: [String] -> BinTree String
makeTree = foldl' (flip insert) Empty

-- | Get the size (number of leaves) of a binary tree
size :: BinTree a -> Int
size Empty = 0
size (Leaf x) = 1
size (Branch s t1 t2) = s

-- | Search a binary tree for the bitstring that corresponds to the desired criteria.
-- NOTE: The "frequency" of a given bit at a current position is given by the
-- subtree sizes, since all bitstrings with '0' in a given position will be to
-- the left of those with '1' in the position.
getRating :: (Int -> Int -> Bool) -> BinTree String -> Maybe String
getRating branchLeft tree = case tree of
    -- if there's only one number left, that's the desired rating.
    (Leaf item) -> Just item
    t@(Branch s l r) ->
      -- if there's only one number in the tree, that's the desired rating. Get it from wherever it is.
      if s == 1 then dive t
      else let zeros = size l
               ones  = size r
            in if zeros + ones == 1 then dive t
               -- else discard the bitstrings that don't satisfy the criteria and go on to the next bit
               else if branchLeft zeros ones then getRating branchLeft l
               else getRating branchLeft r
    -- fail on empty trees
    _ -> Nothing
  where
    -- | Get the first element of a tree, if any
    dive Empty = Nothing
    dive (Leaf x) = Just x
    dive (Branch s l r) = case dive l of
      Nothing -> dive r
      Just x -> Just x

-- | Use the binary numbers in your diagnostic report to calculate the oxygen
-- generator rating and CO2 scrubber rating, then multiply them together.
binaryDiagnostic2 :: String -> String
binaryDiagnostic2 =
    show . uncurry (*) .
    (getOxygenGeneratorRating &&& getCO2ScrubberRating) .
    makeTree . lines
  where
    getOxygenGeneratorRating = toDecimal . getRating (>) -- branch on the most common bit, or right if both are equally common
    getCO2ScrubberRating = toDecimal . getRating (<=) -- branch on the least common bit, or left if both are equally common
    toDecimal = toInteger . bin2dec . fmap C.digitToInt . fromJust -- convert a bitstring to its decimal representation
