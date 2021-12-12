module Day11 (dumboOctopus1, dumboOctopus2) where

import Utils (Matrix, untilEqual, mapMatrix, Rule, applyRule)
import Data.Char (digitToInt)

data Octopus = Padding       -- only for converting into cells
       | JustFlashed   -- flashed in the last small step
       | PrevFlashed   -- flashed before the last small step
       | Unflashed Int -- not yet flashed
  deriving (Eq, Show)

dumboOctopus1 :: String -> String
dumboOctopus1 =
  show
  . fst
  . countAfterSteps 100
  . readInput

countAfterSteps ::  Int -> Matrix Octopus -> (Int, Matrix Octopus)
countAfterSteps n = go n 0
  where
    go 0 c os = (c, os)
    go n c os =
      let (c', os') = stepAndCount c os
       in go (n-1) c' os'

stepAndCount :: Int -> [[Octopus]] -> (Int, Matrix Octopus)
stepAndCount k os =
  let os' = bigStep os
      n = sum (fmap (length . filter (== Unflashed 0)) os')
   in (k + n, os')

bigStep :: Matrix Octopus -> Matrix Octopus
bigStep = reset . untilEqual smallStep . incMatrix
  where
    incMatrix :: [[Octopus]] -> [[Octopus]]
    incMatrix = fmap (fmap $ incLevel 1)

    -- | Reset flashed octopuses to 0 energy
    reset :: Matrix Octopus -> Matrix Octopus
    reset = fmap (fmap f)
      where
        f JustFlashed = Unflashed 0
        f PrevFlashed = Unflashed 0
        f e = e



-- | Increment an octopus by the number of flashed neighbors, marking it as
-- flashed or already flashed, if needed
smallStep :: Matrix Octopus -> Matrix Octopus
smallStep = applyRule Padding flashRule

flashRule :: Rule Octopus
flashRule [above, [before, x, after], below]
  = let n = length $ filter isFlashed (above <> [before, after] <> below)
     in [above, [before, incLevel n x, after], below]
  where
    isFlashed :: Octopus -> Bool
    isFlashed JustFlashed = True
    isFlashed _ = False

flashRule c = error $ "flashRule: invalid cell: " <> show c

incLevel :: Int -> Octopus -> Octopus
incLevel n (Unflashed i) = if i + n > 9 then JustFlashed else Unflashed (i + n)
incLevel n JustFlashed = PrevFlashed
incLevel _ o = o


--
-- Part 2
--

dumboOctopus2 :: String -> String
dumboOctopus2 =
  show
  . firstSimultaneousFlash
  . readInput

firstSimultaneousFlash :: Matrix Octopus -> Integer
firstSimultaneousFlash = go 0
  where
    go n os'
      | all (all (== Unflashed 0)) os' = n
      | otherwise = go (n + 1) (bigStep os')

--
-- Utils
--

readInput :: String -> Matrix Octopus
readInput = mapMatrix (Unflashed . digitToInt) . lines

dumboOctopusExample :: String
dumboOctopusExample =
  unlines [
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526"
  ]