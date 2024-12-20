module Main (main) where

import Data.Maybe (mapMaybe)
import Data.Ratio (numerator, denominator)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Utils (Parser, parseInput)
import Vec (V2(..))

data Machine = Machine {
    buttonA :: !(V2 Int), buttonB :: !(V2 Int), prize :: !(V2 Int)
} deriving Show

buttonP :: Char -> Parser (V2 Int)
buttonP name =
    V2 <$  P.string "Button " <* P.char name <* P.string ": X+"
       <*> P.decimal <* P.string ", Y+"
       <*> P.decimal

prizeP :: Parser (V2 Int)
prizeP =
    V2 <$  P.string "Prize: X="
       <*> P.decimal <* P.string ", Y="
       <*> P.decimal

machineP :: Parser Machine
machineP =
    Machine <$> buttonP 'A' <* P.newline
            <*> buttonP 'B' <* P.newline
            <*> prizeP <* P.newline

inputP :: Parser [Machine]
inputP = machineP `P.sepBy` P.newline

type Matrix = V2 (V2 Rational)

inverse :: Matrix -> Maybe Matrix
inverse (V2 (V2 a b) (V2 c d))
  | det == 0 = Nothing
  | otherwise = Just $ V2 (V2 d (-b)) (V2 (-c) a) / pure (pure det)
  where
    det = a * d - b * c

mulMV :: Matrix -> V2 Rational -> V2 Rational
mulMV (V2 c1 c2) (V2 x y) = pure x * c1 + pure y * c2

getInt :: Rational -> Maybe Int
getInt x
  | d == 1 = Just $ fromIntegral n
  | otherwise = Nothing
  where
    n = numerator x
    d = denominator x

findPresses :: Machine -> Maybe (V2 Int)
findPresses Machine{..} = do
    let m = V2 (fromIntegral <$> buttonA) (fromIntegral <$> buttonB)
        v = fromIntegral <$> prize
    m_inv <- inverse m
    let V2 a b = mulMV m_inv v
    V2 <$> getInt a <*> getInt b

tokensNeeded :: V2 Int -> Int
tokensNeeded (V2 a b) = 3 * a + b

totalTokensNeeded :: [Machine] -> Int
totalTokensNeeded = sum . map tokensNeeded . mapMaybe findPresses

farPrize :: Machine -> Machine
farPrize m = m{ prize = (+ k) <$> prize m }
  where
    k = 10_000_000_000_000

main :: IO ()
main = do
    machines <- parseInput 13 inputP
    print $ totalTokensNeeded machines
    print $ totalTokensNeeded (map farPrize machines)
