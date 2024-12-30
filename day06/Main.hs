module Main (main) where

import qualified Data.HashSet as S
import Data.Massiv.Array (Array, U, Ix2(..))
import qualified Data.Massiv.Array as A
import Data.Maybe (fromMaybe)

import OrphanInstances ()
import Utils (Grid, loadGrid)

type Walls = Array U Ix2 Bool

findStart :: Grid -> Ix2
findStart = fromMaybe (error "No ^ found") . A.findIndex (== '^')

toWalls :: Grid -> Walls
toWalls grid = A.compute @U (A.map isWall grid)
  where
    isWall '.' = False
    isWall '^' = False
    isWall '#' = True
    isWall ch = error $ "Unexpected " ++ show ch

startDir :: Ix2
startDir = -1 :. 0

turnRight :: Ix2 -> Ix2
turnRight (i :. j) = j :. (-i)

data State = State { sPos :: !Ix2, sDir :: !Ix2 }
    deriving (Eq, Show)

class WallsLike a where
    lookupWalls :: a -> Ix2 -> Maybe Bool

instance WallsLike Walls where
    lookupWalls = A.indexM

tracePath :: WallsLike w => w -> Ix2 -> [State]
tracePath walls start = go (State start startDir)
  where
    go s@(State pos dir) = case lookupWalls walls (pos + dir) of
        Nothing -> [s]
        Just True -> s : go (State pos (turnRight dir))
        Just False -> s : go (State (pos + dir) dir)

visitedPos :: Walls -> Ix2 -> [Ix2]
visitedPos walls start =
    S.toList $ S.fromList $ map sPos $ tracePath walls start

data Walls1 = Walls1 Walls Ix2

instance WallsLike Walls1 where
    lookupWalls (Walls1 walls extra) pos
      | pos == extra = Just True
      | otherwise = A.indexM walls pos

addExtra :: Walls -> Ix2 -> [Ix2] -> [Walls1]
addExtra walls start visited =
    map (Walls1 walls) $ filter (/= start) visited

detectLoop :: Eq a => [a] -> Bool
detectLoop stream = go stream stream
  where
    go _ [] = False
    go _ [_] = False
    go [] _ = error "impossible! slow pointer ends before fast pointer"
    go (x : xs) (_ : y : ys)
      | x == y = True
      | otherwise = go xs ys

numLoops :: Walls -> Ix2 -> [Ix2] -> Int
numLoops walls start visited =
    length $ filter (detectLoop . flip tracePath start)
                    (addExtra walls start visited)

main :: IO ()
main = do
    grid <- loadGrid 6
    let start = findStart grid
        walls = toWalls grid
        visited = visitedPos walls start
    print $ length visited
    print $ numLoops walls start visited
