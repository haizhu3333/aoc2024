module Main (main) where

import Data.Massiv.Array (Vector, P, Ix2(..))
import qualified Data.Massiv.Array as A
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Utils (Parser, Grid, parseInput, gridP)

data Pins = Lock (Vector P Int) | Key (Vector P Int)
    deriving Show

gridsP :: Parser [Grid]
gridsP = gridP `P.sepEndBy` P.newline

toPins :: Grid -> Pins
toPins grid
  | full 0 = Lock (heights 1 1)
  | full 6 = Key (heights 5 (-1))
  | otherwise = error "Grid represents neither lock nor key"
  where
    full i = A.all (== '#') $ grid A.!> i
    heights i di = A.compute $ A.generate A.Seq (A.Sz1 5) (go 0 i di)
    go !acc !i !di !j = case grid A.! (i :. j) of
        '.' -> acc
        '#' -> go (acc + 1) (i + di) di j
        ch -> error $ "Invalid char " ++ show ch

partitionPins :: [Pins] -> ([Vector P Int], [Vector P Int])
partitionPins = go [] []
  where
    go ls ks [] = (ls, ks)
    go ls ks (Lock l : ps) = go (l : ls) ks ps
    go ls ks (Key k : ps) = go ls (k : ks) ps

fits :: Vector P Int -> Vector P Int -> Bool
fits l k = A.all (<= 5) $ A.zipWith (+) l k

countFits :: [Grid] -> Int
countFits grid = length $ filter id $ liftA2 fits ls ks
  where
    (ls, ks) = partitionPins $ map toPins grid

main :: IO ()
main = do
    grids <- parseInput 25 gridsP
    print $ countFits grids
