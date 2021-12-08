module Main where

import Control.Exception (IOException, catch)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import Text.Read (readMaybe)
import qualified Day6
import qualified Day7
import qualified Day8

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
  _ -> const Nothing