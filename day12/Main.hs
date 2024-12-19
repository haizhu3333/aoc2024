module Main (main) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Data.Monoid (Sum(..))

import OrphanInstances ()
import Utils (Grid, loadGrid)

indexSet :: Grid -> HashSet Ix2
indexSet = A.ifoldlS (\s ix _ -> S.insert ix s) S.empty

pick :: HashSet a -> Maybe a
pick s = case S.toList s of
    [] -> Nothing
    x : _ -> Just x

dirs :: [Ix2]
dirs = [-1 :. 0, 0 :. -1, 1 :. 0, 0 :. 1]

neighbours1 :: Ix2 -> [Ix2]
neighbours1 ix = [ix + d | d <- dirs]

neighbours :: HashSet Ix2 -> HashSet Ix2
neighbours = foldMap (S.fromList . neighbours1)

floodFill :: Grid -> Ix2 -> HashSet Ix2
floodFill grid pos = go S.empty (S.singleton pos)
  where
    go seen front
      | S.null front = seen
      | otherwise =
        go (S.union front seen) (S.filter (ok seen) (neighbours front)) 
    val = grid A.! pos
    ok seen ix = ix `notElem` seen && (grid A.!? ix) == Just val

edgeCount :: (HashSet Ix2 -> Ix2 -> Int) -> HashSet Ix2 -> Int
edgeCount count ixs = getSum $ foldMap (Sum . count ixs) ixs

scoreAllRegions :: (HashSet Ix2 -> Ix2 -> Int) -> Grid -> Int
scoreAllRegions count grid = go (indexSet grid) 0
  where
    go ixs !total = case pick ixs of
        Nothing -> total
        Just ix ->
            let region = floodFill grid ix
                ixs' = S.difference ixs region
            in go ixs' (total + S.size region * edgeCount count region)

perimeterCount :: HashSet Ix2 -> Ix2 -> Int
perimeterCount ixs ix = sum [1 | nbr <- neighbours1 ix, nbr `notElem` ixs]

rotate :: Ix2 -> Ix2
rotate (di :. dj) = dj :. -di

sideCount :: HashSet Ix2 -> Ix2 -> Int
sideCount ixs ix =
    sum [ 1 | d <- dirs
            , (ix + d) `notElem` ixs
            , let rd = rotate d
            , (ix + rd) `notElem` ixs || (ix + rd + d) `elem` ixs
            ]

main :: IO ()
main = do
    grid <- loadGrid 12
    print $ scoreAllRegions perimeterCount grid
    print $ scoreAllRegions sideCount grid