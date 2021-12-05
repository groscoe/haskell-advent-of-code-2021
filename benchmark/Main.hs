module Main where

import Control.Monad (guard)
import Criterion.Main
import Criterion.Types (Config (timeLimit))
import Data.Functor ((<&>))
import Data.Traversable (for)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import System.Environment (getArgs)

readFileStrict :: FilePath -> IO String
readFileStrict path = do
  contents <- readFile path
  pure $ length contents `seq` contents

runChallenge :: Int -> Int -> (String -> String)
runChallenge 1 1 = Day1.sonarSweep1
runChallenge 1 2 = Day1.sonarSweep2
runChallenge 2 1 = Day2.dive1
runChallenge 2 2 = Day2.dive2
runChallenge 3 1 = Day3.binaryDiagnostic1
runChallenge 3 2 = Day3.binaryDiagnostic2
runChallenge 4 1 = Day4.giantSquid1
runChallenge 4 2 = Day4.giantSquid2
runChallenge 5 1 = Day5.hydrothermalVenture1
runChallenge 5 2 = Day5.hydrothermalVenture2
runChallenge _ _ = error "not solved yet"

main :: IO ()
main = do
  let latestDaySolved = 5
  inputs <- traverse readFile ((\d -> "input/day" <> show d <> ".txt") <$> [1 .. latestDaySolved])
  let benchmarks = flip concatMap (zip [1 ..] inputs) $ \(day, input) ->
        [ bench ("day " <> show day <> ", part 1") $ nf (runChallenge day 1) input,
          bench ("day " <> show day <> ", part 2") $ nf (runChallenge day 2) input
        ]
  defaultMainWith (defaultConfig {timeLimit = 10}) benchmarks