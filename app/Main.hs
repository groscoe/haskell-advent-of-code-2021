module Main where

import Control.Exception (IOException, catch)
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day2
import qualified Day20
import qualified Day21
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Select the desired challenge (1-50): "
  challengeNo <- getLine
  case readMaybe challengeNo of
    Nothing -> putStrLn $ "Invalid challenge number: \"" <> challengeNo <> "\""
    Just n -> solveChallenge n

-- | Output the solution of a challenge, if it exists and has already been solved.
solveChallenge :: Int -> IO ()
solveChallenge n
  | n < 1 || n > 50 = putStrLn "Invalid challenge number. Choose a number between 1 and 50"
  | otherwise = do
    input <- getChallengeInput n
    case input >>= solve n of
      Nothing -> putStrLn "Challenge not solved yet."
      Just answer -> putStrLn $ "--- Answer ---\n" <> answer

-- | Get the input for a challenge from a file in the 'input' folder
getChallengeInput :: Int -> IO (Maybe String)
getChallengeInput n = do
  let inputPath = "./input/day" <> show (getDay n) <> ".txt"
  (Just <$> readFile inputPath)
    `catch` \e -> do
      let err = show (e :: IOException)
      putStrLn $ "Error reading file: " <> err
      pure Nothing
  where
    -- get the day corresponding to the challenge, since the input files are the same
    getDay challengeNo = let (d, r) = challengeNo `divMod` 2 in d + r

-- | Solve a challenge for a given input
solve ::
  -- | Challenge number
  Int ->
  -- | Raw input for the challenge
  String ->
  Maybe String
solve challengeNo = case challengeNo of
  1 -> Just . Day1.sonarSweep1
  2 -> Just . Day1.sonarSweep2
  3 -> Just . Day2.dive1
  4 -> Just . Day2.dive2
  5 -> Just . Day3.binaryDiagnostic1
  6 -> Just . Day3.binaryDiagnostic2
  7 -> Just . Day4.giantSquid1
  8 -> Just . Day4.giantSquid2
  9 -> Just . Day5.hydrothermalVenture1
  10 -> Just . Day5.hydrothermalVenture2
  11 -> Just . Day6.lanternfish1
  12 -> Just . Day6.lanternfish2
  13 -> Just . Day7.treacheryOfWhales1
  14 -> Just . Day7.treacheryOfWhales2
  15 -> Just . Day8.sevenSegmentSearch1
  16 -> Just . Day8.sevenSegmentSearch2
  17 -> Just . Day9.smokeBasin1
  18 -> Just . Day9.smokeBasin2
  19 -> Just . Day10.syntaxScoring1
  20 -> Just . Day10.syntaxScoring2
  21 -> Just . Day11.dumboOctopus1
  22 -> Just . Day11.dumboOctopus2
  23 -> Just . Day12.passagePathing1
  24 -> Just . Day12.passagePathing2
  25 -> Just . Day13.transparentOriginal1
  26 -> Just . Day13.transparentOriginal2
  27 -> Just . Day14.extendedPolymerization1
  28 -> Just . Day14.extendedPolymerization2
  29 -> Just . Day15.chiton1
  30 -> Just . Day15.chiton2
  31 -> Just . Day16.packetDecoder1
  32 -> Just . Day16.packetDecoder2
  33 -> Just . Day17.trickShot1
  34 -> Just . Day17.trickShot2
  35 -> Just . Day18.snailfish1
  36 -> Just . Day18.snailfish2
  37 -> const Nothing
  38 -> const Nothing
  39 -> Just . Day20.trenchMap1
  40 -> Just . Day20.trenchMap2
  41 -> Just . Day21.diracDice1
  _ -> const Nothing