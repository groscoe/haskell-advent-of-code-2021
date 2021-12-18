{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Day18 (snailfish1, snailfish2) where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.List (foldl1')
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP (ReadP, char, many1, (<++))
import Utils (parseNumber, runParser, token)

--
-- * Part 1
--


-- | Add up all of the snailfish numbers from the homework assignment in the
-- order they appear. What is the magnitude of the final sum?
snailfish1 :: String -> String
snailfish1 =
  show
  . magnitude
  . foldl1' (~+~)
  . fromJust
  . parseInput

-- | The magnitude of a pair is 3 times the magnitude of its left element plus 2
-- times the magnitude of its right element. The magnitude of a regular number
-- is just that number.
magnitude :: Num a => SN a -> a
magnitude (RN x) = x
magnitude (P l r) = 3*magnitude l + 2*magnitude r


--
-- * Part 2
--


-- | What is the largest magnitude of any sum of two different snailfish numbers
-- from the homework assignment?
snailfish2 :: String -> String
snailfish2 =
  show
  . maximum
  . fmap (magnitude . uncurry (~+~))
  . selfProd
  . fromJust
  . parseInput
  where
    selfProd xs = [(x, y) | x <- xs, y <- xs]


--
-- * Utils
--


--
-- ** Snail numbers
--


-- | The type of snail numbers.  A simple binary tree with values in the leaves.
data SN a
  = RN a -- ^ A regular number
  | P (SN a) (SN a) -- ^ A pair of snail numbers
  deriving (Eq, Show, Functor)


--
-- *** Parsing and showing
--


parseInput :: String -> Maybe [SN Int]
parseInput =
  traverse (runParser parseSN)
  . lines

parseSN :: ReadP (SN Int)
parseSN = parsePair <++ parseRegularNumber

parseRegularNumber :: ReadP (SN Int)
parseRegularNumber = RN <$> parseNumber

parsePair :: ReadP (SN Int)
parsePair = char '[' *> (P <$> parseSN <*> (char ',' *> parseSN)) <* char ']'

-- | Show a snail number as a list of tuples, for ease of debugging
showSN :: Show a => SN a -> String
showSN (RN x) = show x
showSN (P l r) = "[" <> showSN l <> "," <> showSN r <> "]"


--
-- ** A zipper for snail numbers
--


-- | A zipper for snail numbers
data Location a = Loc (SN a) (Path a) -- ^ A node or leaf and its path in the tree
  deriving (Eq, Show)

-- | A path to a node and its sibling, if it has one.
data Path a
  = Top -- ^ Top of the tree
  | L (Path a) (SN a)  -- ^ The left child of some parent node: L (path to parent) (right sibling)
  | R (SN a) (Path a) -- ^ The right child of some parent node: R (left sibling) (path to parent)
  deriving (Eq, Show)

-- | Modify the focus of the zipper
modify :: (SN a -> SN a) -> Location a -> Location a
modify f (Loc x p) = Loc (f x) p

toLocation :: SN a -> Location a
toLocation x = Loc x Top

fromLocation :: Location a -> SN a
fromLocation loc@(Loc x p) = case p of
  Top -> x
  _ -> fromRight undefined (fromLocation <$> moveUp loc)


--
-- *** Moving around in the tree
--


moveLeft :: Location a -> Either String (Location a)
moveLeft (Loc x p) = case p of
  Top -> Left "left of top"
  R leftSibling pathToParent -> Right $ Loc leftSibling (L pathToParent x)
  L pathToParent rightSibling -> Left "left of leftmost"

moveRight :: Location a -> Either String (Location a)
moveRight (Loc x p) = case p of
  Top -> Left "right of top"
  L pathToParent rightSibling -> Right $ Loc rightSibling (R x pathToParent)
  R rightSibling pathToParent -> Left "right of rightmost"

moveUp :: Location a -> Either String (Location a)
moveUp (Loc x p) = case p of
  Top -> Left "up of top"
  L pathToParent rightSibling -> Right $ Loc (P x rightSibling) pathToParent
  R leftSibling pathToParent -> Right $ Loc (P leftSibling x) pathToParent

moveToTop :: Location a -> Location a
moveToTop loc@(Loc _ p) = case p of
  Top -> loc
  _ -> fromRight undefined $ moveToTop <$> moveUp loc

moveDownLeft :: Location a -> Either String (Location a)
moveDownLeft (Loc x p) = case x of
  RN _ -> Left "down of leaf"
  P leftChild rightChild -> Right $ Loc leftChild (L p rightChild)

moveDownRight :: Location a -> Either String (Location a)
moveDownRight (Loc x p) = case x of
  RN _ -> Left "Down of leaf"
  P leftChild rightChild -> Right $ Loc rightChild (R leftChild p)

-- | Go to the rightmost regular number in the tree
toRightMostNumber :: Location a -> Either String (Location a)
toRightMostNumber loc@(Loc x p) = case x of
  RN _ -> Right loc
  P _ _ -> moveDownRight loc >>= toRightMostNumber

-- | Go to the leftmost regular number in the tree
toLeftMostNumber :: Location a -> Either String (Location a)
toLeftMostNumber loc@(Loc x p) = case x of
  RN _ -> Right loc
  P _ _ -> moveDownLeft loc >>= toLeftMostNumber

-- | Go to the _rightmost_ regular number to the _left_ of the current location
leftNeighbor :: Location a -> Either String (Location a)
leftNeighbor loc@(Loc _ p) = case p of
  Top -> Left "no left neighbor"
  L _ _ -> moveUp loc >>= leftNeighbor
  R _ _ -> moveUp loc >>= moveDownLeft >>= toRightMostNumber

-- | Go to the _leftmost_ regular number to the _right_ of the current location
rightNeighbor :: Location a -> Either String (Location a)
rightNeighbor loc@(Loc _ p) = case p of
  Top -> Left "no right neighbor"
  L _ _ -> moveUp loc >>= moveDownRight >>= toLeftMostNumber
  R _ _ -> moveUp loc >>= rightNeighbor


--
-- *** Movement functions recording the traversed path
--


moveUpP :: Location a -> Either String (Location a, Location a -> Either String (Location a))
moveUpP loc@(Loc x p) = case p of
  Top -> Left "up of top"
  L _ _ -> moveUp loc <&> (, moveDownLeft)
  R _ _ -> moveUp loc <&> (, moveDownRight)

moveRightP :: Location a -> Either String (Location a, Location a -> Either String (Location a))
moveRightP loc = moveRight loc <&> (, moveLeft)

moveLeftP :: Location a -> Either String (Location a, Location a -> Either String (Location a))
moveLeftP loc = moveLeft loc <&> (, moveRight)

toRightMostNumberP :: Location a -> Either String (Location a, Location a -> Either String (Location a))
toRightMostNumberP loc@(Loc x p) = case x of
  RN _ -> Right (loc, pure)
  P _ _ -> do
    (rightMost, backToRightChild)  <- moveDownRight loc >>= toRightMostNumberP
    pure (rightMost, backToRightChild >=> moveUp)

toLeftMostNumberP :: Location a -> Either String (Location a, Location a -> Either String (Location a))
toLeftMostNumberP loc@(Loc x p) = case x of
  RN _ -> Right (loc, pure)
  P _ _ -> do
    (leftMost, backToLeftChild)  <- moveDownLeft loc >>= toLeftMostNumberP
    pure (leftMost, backToLeftChild >=> moveUp)

leftNeighborP :: Location a -> Either String (Location a, Location a -> Either String (Location a))
leftNeighborP loc@(Loc _ p) = case p of
  Top -> Left "no left neighbor"
  L _ _ -> do
    parent <- moveUp loc
    (leftN, backToParent) <- leftNeighborP parent
    pure (leftN, backToParent >=> moveDownLeft)
  R _ _ -> do
    leftSibling <- moveUp loc >>= moveDownLeft
    (leftN, backToLeftSibling) <- toRightMostNumberP leftSibling
    pure (leftN, backToLeftSibling >=> moveUp >=> moveDownRight)

rightNeighborP :: Location a -> Either String (Location a, Location a -> Either String (Location a))
rightNeighborP loc@(Loc _ p) = case p of
  Top -> Left "no right neighbor"
  L _ _ -> do
    rightSibling <- moveUp loc >>= moveDownRight
    (rightN, backToRightSibling) <- toLeftMostNumberP rightSibling
    pure (rightN, backToRightSibling >=> moveUp >=> moveDownLeft)
  R _ _ -> do
    parent <- moveUp loc
    (rightN, backToParent) <- rightNeighborP parent
    pure (rightN, backToParent >=> moveDownRight)


--
-- *** Snail number addition
--

-- | Sum two snail numbers and reduce the result as much as possible.
(~+~) :: (Integral a, Ord a) => SN a -> SN a -> SN a
x ~+~ y = reduce . toLocation $ P x y
  where
    reduce loc = case goToLevelL 4 loc of
      Right loc' -> fromRight undefined $ explodePair loc' <&> reduce . moveToTop
      Left _ -> case goToLeftmostSplittableNumber loc of
        Right loc' -> fromRight undefined $ splitNumber loc' <&> reduce . moveToTop
        Left _ -> fromLocation loc

infixl 6 ~+~

-- | To *explode* a pair, the pair's left value is added to the first regular
-- number to the left of the exploding pair (if any), and the pair's right value
-- is added to the first regular number to the right of the exploding pair (if
-- any). Then, the entire exploding pair is replaced with the regular number 0.
explodePair :: Num a => Location a -> Either String (Location a)
explodePair loc@(Loc x _) = case x of
  P (RN x) (RN y) -> do
    updatedLeft <- case leftNeighborP loc of
      Left _ -> pure loc
      Right (leftN, backHere) -> pure (modify (fmap (+x)) leftN) >>= backHere
    updatedRight <- case rightNeighborP updatedLeft of
      Left _ -> pure updatedLeft
      Right (rightN, backHere) -> pure (modify (fmap (+y)) rightN) >>= backHere
    pure $ modify (const (RN 0)) updatedRight
  _ -> Left "can only explode pairs of regular numbers"

-- | To *split* a regular number, replace it with a pair; the left element of the
-- pair should be the regular number divided by two and rounded down, while the
-- right element of the pair should be the regular number divided by two and
-- rounded up. For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes
-- [6,6], and so on.
splitNumber :: Integral a => Location a -> Either String (Location a)
splitNumber loc@(Loc x _) = case x of
  RN x -> Right $ modify (const $ split x) loc
    where
      split i = let l = i `div` 2 in if odd i then P (RN l) (RN $ l + 1) else P (RN l) (RN l)
  _ -> Left "can only split regular numbers"

-- | Go down to the leftmost pair nested N levels deep
goToLevelL :: Int -> Location a -> Either String (Location a)
goToLevelL 0 loc@(Loc (RN _) _) = Left "reached leaf when going down"
goToLevelL 0 x = Right x
goToLevelL n loc@(Loc x p) = case x of
  RN _ -> Left "can't go lower than leaf"
  P l r -> case moveDownLeft loc >>= goToLevelL (n-1) of
    Left err -> moveDownRight loc >>= goToLevelL (n-1)
    Right newLoc -> Right newLoc

-- | Go to the leftmost number that can be splitted (i.e., that is greater than 10)
goToLeftmostSplittableNumber :: (Num a, Ord a) => Location a -> Either String (Location a)
goToLeftmostSplittableNumber loc@(Loc x _) = case x of
  RN y -> if y >= 10 then Right loc else Left "no splittable number"
  P _ _ -> case moveDownLeft loc >>= goToLeftmostSplittableNumber of
    Left _ -> moveDownRight loc >>= goToLeftmostSplittableNumber
    Right loc' -> Right loc'


--
-- * Input examples
--


-- | The final sum of this list is [[[[1,1],[2,2]],[3,3]],[4,4]]
snailfishExample1 :: String
snailfishExample1 =
  unlines [
    "[1,1]",
    "[2,2]",
    "[3,3]",
    "[4,4]"
  ]

-- | The final sum of this list is [[[[3,0],[5,3]],[4,4]],[5,5]]
snailfishExample2 :: String
snailfishExample2 =
  unlines [
    "[1,1]",
    "[2,2]",
    "[3,3]",
    "[4,4]",
    "[5,5]"
  ]

-- | The final sum of this list is [[[[5,0],[7,4]],[5,5]],[6,6]]
snailfishExample3 :: String
snailfishExample3 =
  unlines [
    "[1,1]",
    "[2,2]",
    "[3,3]",
    "[4,4]",
    "[5,5]",
    "[6,6]"
  ]

-- | The final sum is [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
snailfishExample4 :: String
snailfishExample4 =
  unlines [
    "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
    "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
    "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
    "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
    "[7,[5,[[3,8],[1,4]]]]",
    "[[2,[2,2]],[8,[8,1]]]",
    "[2,9]",
    "[1,[[[9,3],9],[[9,0],[0,7]]]]",
    "[[[5,[7,4]],7],1]",
    "[[[[4,2],2],6],[8,7]]"
  ]

-- | The final sum is [[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]
snailfishExample5 :: String
snailfishExample5 =
  unlines [
    "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
    "[[[5,[2,8]],4],[5,[[9,9],0]]]",
    "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
    "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
    "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
    "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
    "[[[[5,4],[7,7]],8],[[8,3],8]]",
    "[[9,3],[[9,9],[6,[4,9]]]]",
    "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
    "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
  ]
