module Main (main) where

import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Massiv.Array (Matrix, BL, U, Ix2(..))
import qualified Data.Massiv.Array as A

import OrphanInstances ()
import Utils (Grid, loadGrid)

type Topo = Matrix U Int

getTopo :: Grid -> Topo
getTopo = A.compute @U . A.map digitToInt

trailheads :: Topo -> [Ix2]
trailheads topo = do
    let size = A.size topo
    i <- [0 .. A.totalElem size - 1]
    let ix = A.fromLinearIndex size i
    guard $ A.index' topo ix == 0
    pure ix

neighboursAtHeight :: Topo -> Ix2 -> Int -> HashSet Ix2
neighboursAtHeight topo (i :. j) h = S.fromList $ do
    pos <- [i - 1 :. j, i :. j - 1, i + 1 :. j, i :. j + 1]
    guard $ A.indexM topo pos == Just h
    pure pos

allNeighboursAtHeight :: Topo -> HashSet Ix2 -> Int -> HashSet Ix2
allNeighboursAtHeight topo ixs h =
    S.foldl' (\res ix -> S.union res (neighboursAtHeight topo ix h)) S.empty ixs

reachablePeaks :: Matrix U Int -> Ix2 -> HashSet Ix2
reachablePeaks topo start = go (S.singleton start) 0
  where
    go ixs 9 = ixs
    go ixs h = go (allNeighboursAtHeight topo ixs (h + 1)) (h + 1)

sumReachablePeaks :: Topo -> Int
sumReachablePeaks topo =
    sum $ map (S.size . reachablePeaks topo) (trailheads topo)

pathCounts :: Topo -> Matrix BL Int
pathCounts topo = table
  where
    table = A.compute $ A.imap count topo

    count _ 9 = 1
    count ix h = S.foldl' (\res nbr -> res + A.index' table nbr) 0
                          (neighboursAtHeight topo ix (h + 1))

sumPathCounts :: Topo -> Int
sumPathCounts topo = sum $ map (A.index' table) (trailheads topo)
  where
    table = pathCounts topo

main :: IO ()
main = do
    grid <- loadGrid 10
    let topo = getTopo grid
    print $ sumReachablePeaks topo
    print $ sumPathCounts topo