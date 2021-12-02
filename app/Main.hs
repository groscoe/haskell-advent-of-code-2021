module Main where

import Text.Read (readMaybe)
import System.IO as S

import Control.Exception (catch, IOException)
import qualified Day1


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
  let inputPath = "./input/" <> show n <> ".txt"
  (Just <$> readFile inputPath)
    `catch` \e -> do
      let err = show (e :: IOException)
      putStrLn $ "Error reading file: " <> err
      pure Nothing


-- | Solve a challenge for a given input
solve :: Int -- ^ Challenge number
  -> String -- ^ Raw input for the challenge
  -> Maybe String
solve challengeNo = case challengeNo of
  1 -> Just . Day1.sonarSweep1
  2 -> Just . Day1.sonarSweep2
  _ -> const Nothing