module Day3 (binaryDiagnostic1) where

import Data.Foldable (Foldable (foldl'))
import Utils (both, bin2dec)
import Data.Char (digitToInt)

import Debug.Trace (traceShow)

--
-- Part 1
--

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

-- The challenge becomes easier if we put the numbers into a "trie" of sorts, so
-- we don't need to "filter" the strings that don't conform to the criteria (for
-- bits, a binary tree sufices). The numbers are stored at the leaves. We start
-- at the root and proceed digit by digit. At each branch, if the next bit is
-- zero, we continue to the left subtree. If the next bit is one, we continue to
-- the right. When we reach the end of the string, we must've reached a leaf
-- containing our number.

-- | A binary tree, with values stored in the leaves
data BinTree a
  = Empty
  | Leaf a
  | Branch (BinTree a) (BinTree a)
  deriving Show

-- | A 'branching cursor', indicating to which subtree we should go next.
data TreeCursor a
  = TLeft a
  | TRight a
  | End
  deriving (Eq, Show)

-- | Insert an item into a 'BinTree' according to a function that computes the
-- next 'TreeCursor'
insert :: (a -> TreeCursor a) -> BinTree a -> a -> BinTree a
insert advance tree item = go item item tree
  where
    go item cursor tree = case (advance cursor, tree) of
      -- If we've reached the end of the cursor and there are no branches, we
      -- store the item
      (End, Empty) -> Leaf cursor
      (End, Leaf _) -> Leaf cursor
      -- Else we branch according to the cursor, creating subtrees if needed.
      (TLeft next, Empty) -> Branch (go item next Empty) Empty
      (TRight next, Empty) -> Branch Empty (go item next Empty)
      (TLeft next, Branch l r) -> Branch (go item next l) r
      (TRight next, Branch l r) -> Branch l (go item next r)
      -- Trying to branch when we've reached a 'Leaf' or reaching a branch at
      -- the end of the cursor is an error and cannot happen if the input is
      -- well-formed (if we tried to go on, we either wouldn't know where to
      -- go, or we'd end up overwriting a leaf with a branch).
      _ -> error "Couldn't insert item in tree"

-- | Compute a branching cursor from a bit string
classifyBit :: String -> TreeCursor String
classifyBit "" = End
classifyBit ('0':bs) = TLeft bs
classifyBit ('1':bs) = TRight bs
classifyBit s = error $ "Unrecognized bit: " <> s

-- | Store a list of bit strings into a trie
makeTree :: [String] -> BinTree String
makeTree = foldl' (insert classifyBit) Empty

-- | We make the BinTree foldable, so we can reuse 'bitFrequencies' to compute
-- the frequencies in a given sub-tree
instance Foldable BinTree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Branch l r) = foldMap f l <> foldMap f r


getRating :: Int -- ^ Number of bits in a bitstring
  -> (Int -> Int -> Either () ()) -- ^ A comparison function of the relative frequencies. Returns the direction of the next subtree.
  -> BinTree String -- ^ A trie of bit-strings
  -> String -- ^ A bit-string with the rating
getRating numBits criteria = go 0
  where
    -- | Search for the rating in the trie
    go :: Int -> BinTree String -> String
    -- if we only have one number left, stop
    go _ (Leaf rating) = rating
    -- Otherwise, discard numbers which do not match the bit criteria (i.e.
    -- branch on the tree) and repeat the process, considering the next bit to
    -- the right
    go i t@(Branch l r) | i <= numBits =
      let (zeros, ones) = bitFrequencies numBits t !! i -- TODO: optimize this, maybe storing the frequencies in the branches
       in case criteria zeros ones of
         Left _ -> try (i + 1) l t
         Right _ -> try (i + 1) r t

    go _ t = traceShow t error "Reached the end of a branch without finding the rating"

    try :: Int -> BinTree String -> BinTree String -> String
    try i Empty t = dive t
    try i t _ = go (i + 1) t

    dive :: BinTree a -> a
    dive = head . foldr (:) []

-- | To find oxygen generator rating, determine the most common value (0 or 1)
-- in the current bit position, and keep only numbers with that bit in that
-- position. If 0 and 1 are equally common, keep values with a 1 in the position
-- being considered.
oxygenCriteria :: Int -> Int -> Either () ()
oxygenCriteria zeros ones
  | ones >= zeros = Right ()
  | otherwise = Left ()

-- | To find CO2 scrubber rating, determine the least common value (0 or 1) in
-- the current bit position, and keep only numbers with that bit in that
-- position. If 0 and 1 are equally common, keep values with a 0 in the position
-- being considered.
co2Criteria :: Int -> Int -> Either () ()
co2Criteria zeros ones
  | ones >= zeros = Left ()
  | otherwise = Right ()
