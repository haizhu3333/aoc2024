module Main (main) where

import Data.Massiv.Array (Ix2(..), (!?))
import Data.Maybe (fromMaybe)
import qualified Data.Massiv.Array as A

import Utils (Grid, loadGrid)

dirs :: [Ix2]
dirs = [i :. j | i <- [-1 .. 1], j <- [-1 .. 1], not (i == 0 && j == 0)]

isXMAS :: Grid -> Ix2 -> Ix2 -> Bool
isXMAS grid pos dir = fromMaybe False $ do
    let ixs = take 4 $ iterate (+ dir) pos
    word <- mapM (grid !?) ixs
    pure $ word == "XMAS"

countXMAS :: Grid -> Int
countXMAS grid = A.sum $ A.imap (const . count) grid
  where
    count :: Ix2 -> Int
    count pos = length $ filter (isXMAS grid pos) dirs

isMidMAS :: Grid -> Ix2 -> Ix2 -> Bool
isMidMAS grid pos dir = fromMaybe False $ do
    word <- mapM (grid !?) [pos - dir, pos, pos + dir]
    pure $ word == "MAS" || word == "SAM"

isCrossMAS :: Grid -> Ix2 -> Bool
isCrossMAS grid pos =
    isMidMAS grid pos (1 :. 1) && isMidMAS grid pos (1 :. -1)

countCrossMAS :: Grid -> Int
countCrossMAS grid = A.sum $ A.imap (const . fromEnum . isCrossMAS grid) grid

main :: IO ()
main = do
    grid <- loadGrid 4
    print $ countXMAS grid
    print $ countCrossMAS grid