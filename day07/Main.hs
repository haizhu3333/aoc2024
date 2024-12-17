module Main (main) where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Utils (Parser, parseInput)

data Equation = Equation { lhs :: Int, rhs :: [Int] }
    deriving Show

equationP :: Parser Equation
equationP = Equation <$> P.decimal <* P.string ": "
                     <*> P.decimal `P.sepBy` P.char ' '

equationsP :: Parser [Equation]
equationsP = equationP `P.sepEndBy` P.newline

type Operator = Int -> Int -> Int

canSolve :: [Operator] -> Equation -> Bool
canSolve ops eq = case rhs eq of
    [] -> error "RHS is empty"
    x : xs -> go (lhs eq) x xs
  where
    go y x0 [] = x0 == y
    go y x0 (x1 : xs) = x0 <= y && or [go y (op x0 x1) xs | op <- ops]  

sumSolvable :: [Operator] -> [Equation] -> Int
sumSolvable ops eqs = sum [lhs eq | eq <- eqs, canSolve ops eq]

catInt :: Operator
catInt x y = x * go 1 + y
  where
    go m | m > y = m
         | otherwise = go (m * 10)

main :: IO ()
main = do
    eqs <- parseInput 7 equationsP
    print $ sumSolvable [(+), (*)] eqs
    print $ sumSolvable [(+), (*), catInt] eqs