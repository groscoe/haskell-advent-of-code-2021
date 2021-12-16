{-# LANGUAGE LambdaCase #-}
module Day16 (packetDecoder1, packetDecoder2) where
import Text.ParserCombinators.ReadP (ReadP, pfail, get, many, char, many1)
import Control.Monad (replicateM)
import Utils ( runParser )
import Data.Foldable (foldl')
import Data.Maybe (fromJust)

--
-- Part1
--

-- | Decode the structure of your hexadecimal-encoded BITS transmission; what do
-- you get if you add up the version numbers in all packets?
packetDecoder1 :: String -> String
packetDecoder1 =
  show
  . sumVersions
  . fromJust
  . parseInput
  where
    sumVersions p@LiteralPacket{} = pVersion p
    sumVersions p@OperatorPacket{} = pVersion p + sum (sumVersions <$> pSubpackets p)

--
-- Part 2
--

-- | Decode the structure of your hexadecimal-encoded BITS transmission; what do
-- you get if you evaluate the expression represented by your
-- hexadecimal-encoded BITS transmission?
packetDecoder2 :: String -> String
packetDecoder2 =
  show
  . evaluatePacket
  . fromJust
  . parseInput

evaluatePacket :: Packet Int -> Int
evaluatePacket p@LiteralPacket{} = pContents p
evaluatePacket p@OperatorPacket{} = apply (pOperation p) (evaluatePacket <$> pSubpackets p)
  where
    apply = \case
      Sum -> sum
      Product -> product
      Minimum -> minimum
      Maximum -> maximum
      GreaterThan -> \[x, y] -> if x > y then 1 else 0
      LessThan -> \[x, y] -> if x < y then 1 else 0
      Equal -> \[x, y] -> if x == y then 1 else 0

--
-- Utils
--

type Bit = Bool
type Bits = [Bit]

data PacketOperation
  = Sum
  | Product
  | Minimum
  | Maximum
  | GreaterThan
  | LessThan
  | Equal
  deriving (Eq, Show)

data Packet a = LiteralPacket { pVersion :: Int , pType :: Int, pContents :: a }
              | OperatorPacket { pVersion :: Int, pType :: Int, pOperation :: PacketOperation, pSubpackets :: [Packet a]}
  deriving (Eq, Show)

parseInput :: String -> Maybe (Packet Int)
parseInput =
  runParser (parsePacket <* many (char '0'))
  . hexToBinaryString

parseLiteralBits :: ReadP Bits
parseLiteralBits = do
  groupType <- get
  case groupType of
    '0' -> parseBitGroup -- end of group
    '1' -> (<>) <$> parseBitGroup <*> parseLiteralBits -- more groups to follow
    _ -> pfail
  where
    parseBitGroup = getBits 4

parseLiteral :: ReadP Int
parseLiteral = toInt <$> parseLiteralBits


parsePacketHeader :: ReadP (Int, Int)
parsePacketHeader = do
  v <- toInt <$> getBits 3
  t <- toInt <$> getBits 3
  pure (v, t)

parseLiteralPacket :: Int -> ReadP (Packet Int)
parseLiteralPacket v = LiteralPacket v 4 <$> parseLiteral

parseOperatorPacket :: Int -> Int -> ReadP (Packet Int)
parseOperatorPacket v t = do
  lengthType <- get
  subpackets <- if lengthType == '0'
    then do
      totalSubpacketLength <- toInt <$> getBits 15
      encodedSubpackets <- replicateM totalSubpacketLength get
      maybe pfail pure $ runParser (many1 parsePacket) encodedSubpackets
    else do
      numSubpackets <- toInt <$> getBits 11
      replicateM numSubpackets parsePacket
  pure $ OperatorPacket v t (operationFromType t) subpackets
  where
    operationFromType = \case
      0 -> Sum
      1 -> Product
      2 -> Minimum
      3 -> Maximum
      5 -> GreaterThan
      6 -> LessThan
      7 -> Equal
      c -> error $ "invalid operation ID: " <> show c


parsePacket :: ReadP (Packet Int)
parsePacket = do
  (v, t) <- parsePacketHeader
  if t == 4 then parseLiteralPacket v else parseOperatorPacket v t

getBits :: Int -> ReadP [Bit]
getBits n = fmap toBit <$> replicateM n get

toBit :: Char -> Bit
toBit = (== '1')

toInt :: [Bit] -> Int
toInt = foldl' (\n b -> (if b then 1 else 0) + 2 * n) 0

hexToBinaryString :: String -> String
hexToBinaryString = concatMap asBits
  where
    asBits = \case
      '0' -> "0000"
      '1' -> "0001"
      '2' -> "0010"
      '3' -> "0011"
      '4' -> "0100"
      '5' -> "0101"
      '6' -> "0110"
      '7' -> "0111"
      '8' -> "1000"
      '9' -> "1001"
      'A' -> "1010"
      'B' -> "1011"
      'C' -> "1100"
      'D' -> "1101"
      'E' -> "1110"
      'F' -> "1111"
      c -> error $ "invalid hex digit: " <> [c]

-- | this packet represents a literal value with binary representation
-- 011111100101, which is 2021 in decimal.
packetDecoderExample1 :: String
packetDecoderExample1 = "D2FE28"

-- | an operator packet with length type ID
-- 0 that contains two sub-packets (literals 10 and 20).
packetDecoderExample2 :: String
packetDecoderExample2 = "38006F45291200"

-- | an operator packet with length type ID
-- 1 that contains three sub-packets (literals 1, 2 and 3)
packetDecoderExample3 :: String
packetDecoderExample3 = "EE00D40C823060"

-- | an operator packet that contains an operator packet that contains an
-- operator packet that contains five literal values; it has a version sum of
-- 31.
packetDecoderExample4 :: String
packetDecoderExample4 = "A0016C880162017C3686B18A3D4780"
