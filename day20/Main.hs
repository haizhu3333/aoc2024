module Main (main) where

import Control.Monad (guard, mzero)
import Data.Foldable (find)
import qualified Data.HashMap.Strict as M
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Data.Maybe (fromMaybe)

import OrphanInstances ()
import Utils (Grid, loadGrid)

type Track = M.HashMap Ix2 Int

getTrack :: Grid -> Track
getTrack grid = go (M.singleton end 0) end 1
  where
    end = fromMaybe (error "No E") $ A.findIndex (== 'E') grid

    nextSpace res pos = not $ M.member pos res || A.index' grid pos == '#'

    go res pos d = case find (nextSpace res) (map (pos +) dirs) of
        Nothing -> error $ "Path broken at " ++ show pos
        Just pos' ->
            let res' = M.insert pos' d res in
            if A.index' grid pos' == 'S'
            then res'
            else go (M.insert pos' d res) pos' (d + 1)


dirs :: [Ix2]
dirs = [-1 :. 0, 1 :. 0, 0 :. -1, 0 :. 1]

distance :: Ix2 -> Ix2 -> Int
distance (ia :. ja) (ib :. jb) = abs (ia - ib) + abs (ja - jb)

shortCheats :: Track -> [(Ix2, Ix2, Int)]
shortCheats track = do
    (begin, beginT) <- M.toList track
    dir <- dirs
    let end = begin + 2 * dir
    case M.lookup end track of
        Nothing -> mzero
        Just endT -> do
            let saved = beginT - endT - 2
            guard $ saved > 0
            pure (begin, end, saved)

longCheats :: Track -> [(Ix2, Ix2, Int)]
longCheats track = do
    (begin, beginT) <- pts
    (end, endT) <- pts
    guard $ beginT > endT
    let d = distance begin end
        saved = beginT - endT - d
    guard $ d <= 20 && saved > 0
    pure (begin, end, saved)
  where
    pts = M.toList track

count100 :: [(a, b, Int)] -> Int
count100 = length . filter (\(_, _, x) -> x >= 100)

main :: IO ()
main = do
    grid <- loadGrid 20
    let track = getTrack grid
    print $ count100 $ shortCheats track
    print $ count100 $ longCheats track
