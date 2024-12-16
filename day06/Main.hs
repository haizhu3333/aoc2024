{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Massiv.Array (Ix2(..), U, Array, B, Ix1)
import qualified Data.Massiv.Array as A
import Data.Maybe (fromMaybe)

import Utils (Grid, loadGrid, chr8)
import Control.Monad (forM_, guard)

findStart :: Grid -> Ix2
findStart = fromMaybe (error "No ^ found") . A.findIndex (\x -> chr8 x == '^')

toWalls :: Grid -> Array U Ix2 Bool
toWalls grid = A.compute @U (A.map isWall grid)
  where
    isWall x = case chr8 x of
        '.' -> False
        '^' -> False
        '#' -> True
        ch -> error $ "Unexpected " ++ show ch

startDir :: Ix2
startDir = -1 :. 0

turnRight :: Ix2 -> Ix2
turnRight (i :. j) = j :. (-i)

tracePath :: Array U Ix2 Bool -> Ix2 -> HashSet (Int, Int)
tracePath walls start = go start startDir S.empty
  where
    go pos dir acc =
        let pos' = pos + dir
            acc' = S.insert (A.fromIx2 pos) acc
        in  case A.indexM walls pos' of
                Nothing -> acc'
                Just True -> go pos (turnRight dir) acc
                Just False -> go pos' dir acc'

data Walls1 = Walls1 (Array U Ix2 Bool) Ix2

indexWalls1 :: Walls1 -> Ix2 -> Maybe Bool
indexWalls1 (Walls1 walls w) pos
    | w == pos = Just True
    | otherwise = A.indexM walls pos

addWall :: Array U Ix2 Bool -> Ix2 -> Array B Ix1 Walls1
addWall walls start = A.fromList A.Par $ do
    posPair <- S.toList $ tracePath walls start
    let pos = A.toIx2 posPair
    guard $ pos /= start
    pure $ Walls1 walls pos 

isLooping :: Walls1 -> Ix2 -> Bool
isLooping walls start = go S.empty start startDir
  where
    go seen pos dir
      | toQuad pos dir `elem` seen = True
      | otherwise =
        let pos' = pos + dir
            seen' = S.insert (toQuad pos dir) seen
        in  case indexWalls1 walls pos' of
                Nothing -> False
                Just True -> go seen' pos (turnRight dir)
                Just False -> go seen' pos' dir
    toQuad (i :. j) (di :. dj) = (i, j, di, dj)

loopMakingOptions :: Array U Ix2 Bool -> Ix2 -> Int
loopMakingOptions walls start =
    A.sum $ A.map (fromEnum . flip isLooping start) (addWall walls start)

main :: IO ()
main = do
    grid <- loadGrid 6
    let start = findStart grid
        walls = toWalls grid
    print $ S.size $ tracePath walls start
    -- Very slow! around 2m23s even with parallelization
    print $ loopMakingOptions walls start