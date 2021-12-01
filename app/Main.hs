module Main where

import Text.Read (readMaybe)

import Day1

main :: IO ()
main = do
  putStrLn "Select the desired challenge (1-50): "
  challengeNo <- getLine
  case readMaybe challengeNo of
    Nothing -> putStrLn $ "Invalid challenge number: \"" <> challengeNo <> "\""
    Just n -> case solveChallenge n of
      Nothing -> putStrLn "Invalid challenge or not solved yet"
      Just answer -> putStrLn $ "--- Answer:---\n" <> answer


-- | Output the solution of a challenge, if it exists and has already been solved.
solveChallenge :: Int -> Maybe String
solveChallenge _ = Nothing

