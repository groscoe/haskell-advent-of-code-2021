{-# LANGUAGE LambdaCase #-}

module Day10 (syntaxScoring1, syntaxScoring2) where

import Data.Either (lefts)
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)
import Stack (Stack, emptyStack, push, remove, stackToList, top)
import Text.ParserCombinators.ReadP (ReadP, char, get, look, optional)
import Utils (halve, runParser)

--
-- Part 1
--
syntaxScoring1 :: String -> String
syntaxScoring1 =
  show
    . sum
    . fmap getScore
    . filter isInvalid
    . lefts
    . fmap readUntilError
    . lines
  where
    getScore :: ParseError -> Int
    getScore (WrongDelimiter _ c) = case c of
      ")" -> 3
      "]" -> 57
      "}" -> 1197
      ">" -> 25137
      _ -> error $ "getScore: invalid character: " <> c
    getScore _ = 0

--
-- Part 2
--

syntaxScoring2 :: String -> String
syntaxScoring2 =
  show
    . getMiddle
    . sort
    . fmap (\(Incomplete stack) -> getScore $ getClosing <$> stackToList stack)
    . filter isIncomplete
    . lefts
    . fmap readUntilError
    . lines
  where
    getScore :: String -> Int
    getScore = foldl' (\score c -> 5 * score + scoreFromChar c) 0

    scoreFromChar :: Char -> Int
    scoreFromChar = \case
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      c -> error $ "scoreFromChar: invalid character: " <> show c

    getMiddle :: [a] -> a
    getMiddle xs = let (_, y : _) = halve xs in y

--
-- Utils
--

isOpening :: Char -> Bool
isOpening = \case
  '(' -> True
  '[' -> True
  '{' -> True
  '<' -> True
  _ -> False

isClosing :: Char -> Bool
isClosing = \case
  ')' -> True
  ']' -> True
  '}' -> True
  '>' -> True
  _ -> False

getClosing :: Char -> Char
getClosing = \case
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'
  c -> error $ "getClosing: invalid delimiter: " <> [c]

data ParseError
  = Incomplete (Stack Char)
  | WrongDelimiter String String
  deriving (Show)

isIncomplete :: ParseError -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _ = False

isInvalid :: ParseError -> Bool
isInvalid (WrongDelimiter _ _) = True
isInvalid _ = False

readNext :: Stack Char -> String -> (Stack Char, Either ParseError String)
readNext [] [] = ([], Right [])
readNext s [] = (s, Left $ Incomplete s)
readNext [] (c : cs)
  | isOpening c {- trace ("pushing: " <> [c]) -} = (push c emptyStack, Right cs)
  | otherwise = (emptyStack, Left $ WrongDelimiter "" [c])
readNext s (c : cs)
  | isOpening c {- trace ("pushing: " <> [c]) -} = (push c s, Right cs)
  | c == getClosing (top s {- trace ("popping: " <> [c]) -}) = (remove s, Right cs)
  | otherwise = (s, Left $ WrongDelimiter [top s] [c])

readUntilError :: String -> Either ParseError String
readUntilError = go emptyStack
  where
    go [] [] = Right ""
    go s cs = case readNext s cs of
      ([], Right []) -> Right ""
      (s, Right cs') -> go s cs'
      (_, Left err) -> Left err

syntaxScoringExample :: String
syntaxScoringExample =
  unlines
    [ "[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "{([(<{}[<>[]}>{[]{[(<()>",
      "(((({<>}<{<{<>}{[]{[]{}",
      "[[<[([]))<([[{}[[()]]]",
      "[{[{({}]{}}([{[{{{}}([]",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "[<(<(<(<{}))><([]([]()",
      "<{([([[(<>()){}]>(<<{{",
      "<{([{{}}[<[[[<>{}]]]>[]]"
    ]