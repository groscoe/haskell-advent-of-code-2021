module Main where

import Criterion.Main
import Criterion.Types (Config (timeLimit))
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

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
runChallenge 6 1 = Day6.lanternfish1
runChallenge 6 2 = Day6.lanternfish2
runChallenge 7 1 = Day7.treacheryOfWhales1
runChallenge 7 2 = Day7.treacheryOfWhales2
runChallenge 8 1 = Day8.sevenSegmentSearch1
runChallenge 8 2 = Day8.sevenSegmentSearch2
runChallenge 9 1 = Day9.smokeBasin1
runChallenge 9 2 = Day9.smokeBasin2
runChallenge 10 1 = Day10.syntaxScoring1
runChallenge 10 2 = Day10.syntaxScoring2
runChallenge 11 1 = Day11.dumboOctopus1
runChallenge 11 2 = Day11.dumboOctopus2
runChallenge 12 1 = Day12.passagePathing1
runChallenge 12 2 = Day12.passagePathing2
runChallenge 13 1 = Day13.transparentOriginal1
runChallenge 13 2 = Day13.transparentOriginal2
runChallenge 14 1 = Day14.extendedPolymerization1
runChallenge 14 2 = Day14.extendedPolymerization2
runChallenge 15 1 = Day15.chiton1
runChallenge 15 2 = Day15.chiton2
runChallenge 16 1 =  Day16.packetDecoder1
runChallenge 16 2 =  Day16.packetDecoder2
runChallenge 17 1 =  Day17.trickShot1
runChallenge 17 2 =  Day17.trickShot2
runChallenge _ _ = error "not solved yet"

main :: IO ()
main = do
  let latestDaySolved = 14
  inputs <- traverse readFile ((\d -> "input/day" <> show d <> ".txt") <$> [1 .. latestDaySolved])
  let benchmarks = flip concatMap (zip [1 ..] inputs) $ \(day, input) ->
        [ bench ("day " <> show day <> ", part 1") $ nf (runChallenge day 1) input,
          bench ("day " <> show day <> ", part 2") $ nf (runChallenge day 2) input
        ]
  defaultMainWith (defaultConfig {timeLimit = 10}) benchmarks