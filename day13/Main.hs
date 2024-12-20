module Main (main) where

import Data.Maybe (mapMaybe)
import Data.Ratio (numerator, denominator)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Utils (Parser, parseInput)

data V a = V !a !a deriving (Show, Functor)

instance Applicative V where
    pure x = V x x
    V f g <*> V x y = V (f x) (g y)

data Machine = Machine {
    buttonA :: !(V Int), buttonB :: !(V Int), prize :: !(V Int)
} deriving Show

buttonP :: Char -> Parser (V Int)
buttonP name =
    V <$  P.string "Button " <* P.char name <* P.string ": X+"
      <*> P.decimal <* P.string ", Y+"
      <*> P.decimal

prizeP :: Parser (V Int)
prizeP =
    V <$  P.string "Prize: X="
      <*> P.decimal <* P.string ", Y="
      <*> P.decimal

machineP :: Parser Machine
machineP =
    Machine <$> buttonP 'A' <* P.newline
            <*> buttonP 'B' <* P.newline
            <*> prizeP <* P.newline

inputP :: Parser [Machine]
inputP = machineP `P.sepBy` P.newline

data Mat = Mat !(V Rational) !(V Rational) deriving Show

inverse :: Mat -> Maybe Mat
inverse (Mat (V a b) (V c d))
  | det == 0 = Nothing
  | otherwise = Just $ Mat (V (d / det) (- b / det)) (V (- c / det) (a / det))
  where
    det = a * d - b * c

mulMV :: Mat -> V Rational -> V Rational
mulMV (Mat c1 c2) (V x y) = (+) <$> fmap (* x) c1 <*> fmap (* y) c2

getInt :: Rational -> Maybe Int
getInt x
  | d == 1 = Just $ fromIntegral n
  | otherwise = Nothing
  where
    n = numerator x
    d = denominator x

findPresses :: Machine -> Maybe (V Int)
findPresses Machine{..} = do
    let m = Mat (fromIntegral <$> buttonA) (fromIntegral <$> buttonB)
        v = fromIntegral <$> prize
    m_inv <- inverse m
    let V a b = mulMV m_inv v
    V <$> getInt a <*> getInt b

tokensNeeded :: V Int -> Int
tokensNeeded (V a b) = 3 * a + b

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
