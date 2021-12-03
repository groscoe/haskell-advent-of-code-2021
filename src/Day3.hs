module Day3 (binaryDiagnostic1) where

import Data.Foldable (Foldable (foldl'))
import Utils (both)

--
-- Part 1
--

type Rates = (Int, Int) -- gamma and epsilon rate, respectively

binaryDiagnostic1 :: String -> String
binaryDiagnostic1 =
  show
    . multiplyRates
    . ratesFromBitFrequencies
    . bitFrequencies
    . lines
  where
    multiplyRates :: Rates -> Int
    multiplyRates (gamma, epsilon) = gamma * epsilon

-- | Compute the gamma and epsilon rates from the bit frequencies
ratesFromBitFrequencies :: [(Int, Int)] -- ^ Counts of zeros and ones
  -> Rates
ratesFromBitFrequencies = both bin2dec . unzip . fmap toBits
  where
    -- | Convert a little-endian list of bits to an integer
    bin2dec :: [Int] -> Int
    bin2dec = foldl' (\n b -> b + 2*n) 0 -- a.k.a <https://en.wikipedia.org/wiki/Horner%27s_method Horner's method>

    -- | Convert pairs of frequencies to pairs of (most common bit, least common bit)
    toBits :: (Int, Int) -> (Int, Int)
    toBits (zeros, ones) = if zeros > ones then (0, 1) else (1, 0)

-- | Count the most common and least common bits in each position of the report
bitFrequencies :: [String] -- ^ A list of bit strings
  -> [(Int, Int)] -- ^ counts of zeros and ones
bitFrequencies [] = []
bitFrequencies report@(firstLine : rest) =
  foldl' updateFrequencies (initializeFrequencies firstLine) report
  where
    updateFrequencies :: [(Int, Int)] -> String -> [(Int, Int)]
    updateFrequencies = zipWith updatePosition

    updatePosition :: (Int, Int) -> Char -> (Int, Int)
    updatePosition (zeros, ones) bit = case bit of
      '0' -> (zeros + 1, ones)
      '1' -> (zeros, ones + 1)
      _ -> error $ "invalid bit: " <> show bit

    initializeFrequencies :: String -> [(Int, Int)]
    initializeFrequencies = fmap (const (0, 0))

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
