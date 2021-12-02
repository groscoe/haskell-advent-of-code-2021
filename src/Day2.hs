{-# LANGUAGE LambdaCase #-}

module Day2 (dive1, dive2) where

import Data.Char (isDigit, isSpace)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    manyTill,
    munch1,
    optional,
    skipSpaces,
    string,
    (+++),
  )
import Utils (runParser)

--
-- Part 1
--

data Command
    = Forward Integer
    | Down Integer
    | Up Integer
    deriving (Eq, Show)

data Position = Position {
    horizontal :: Integer,
    vertical :: Integer
} deriving Show

-- | Calculate the horizontal position and depth you would have after following
-- the planned course. What do you get if you multiply your final horizontal
-- position by your final depth?
dive1 :: String -> String
dive1 = diveWith calculatePosition1

-- | Get the final multiplication value after running all commands through a
-- given interpreter
diveWith :: ([Command] -> Position) -> String -> String
diveWith interpreter =
    show .
    multiplyPosition . interpreter . fromMaybe [] . runParser parseCommands
  where
    multiplyPosition :: Position -> Integer
    multiplyPosition (Position x y) = x * y


-- | Read a command from a string
parseCommands :: ReadP [Command]
parseCommands = manyTill (parse <* optional (char '\n')) eof
  where
    parse :: ReadP Command
    parse = do
        direction <- parseDirection
        skipSpaces
        direction <$> parseLength

    parseDirection :: ReadP (Integer -> Command)
    parseDirection = parseForward +++ parseDown +++ parseUp

    parseForward :: ReadP (Integer -> Command)
    parseForward = string "forward" $> Forward

    parseDown :: ReadP (Integer -> Command)
    parseDown = string "down" $> Down

    parseUp :: ReadP (Integer -> Command)
    parseUp = string "up" $> Up

    parseLength :: ReadP Integer
    parseLength = do
        digits <- munch1 isDigit
        pure $ read digits

-- | Get the final position after a series of commands
calculatePosition1 :: [Command] -> Position
calculatePosition1 = foldl' interpret1 (Position 0 0)


-- | Update a position with a command according to the first instruction set
interpret1 :: Position -> Command -> Position
interpret1 (Position x y) = \case
  Forward n -> Position (x + n) y -- increases the horizontal position by n units.
  Up n -> Position x (y - n)      -- increases the depth by n units.
  Down n -> Position x (y + n)    -- decreases the depth by n units.


exampleCommands :: String
exampleCommands =
  unlines
    [ "forward 5",
      "down 5",
      "forward 8",
      "up 3",
      "down 8",
      "forward 2"
    ]

--
-- part 2
--

-- calculate the horizontal position and depth you would have after following
-- the planned course. What do you get if you multiply your final horizontal
-- position by your final depth?
dive2 :: String -> String
dive2 = diveWith calculatePosition2

-- | Get the final position after a series of commands, according to the second
-- instruction set
calculatePosition2 :: [Command] -> Position
calculatePosition2 = fst . foldl' interpret2 (Position 0 0, 0)

-- | Update a position with a command according to the second instruction set
interpret2 :: (Position, Integer) -> Command -> (Position, Integer)
interpret2 (Position x y, aim) = \case
  Down n -> (Position x y, aim + n) -- "down n" *increases* your aim by n units.
  Up n -> (Position x y, aim - n)   -- "up n" *decreases* yout aim by n units.
  Forward n ->                      -- "forward n" does two things:
    -- -It increases your horizontal position by n units.
    let newX = x + n
    -- -It increases your depth by your aim multiplied by n.
        newY = y + (aim * n)
     in (Position newX newY, aim)
