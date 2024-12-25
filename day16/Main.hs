module Main (main) where

import Control.Monad (guard)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.HashPSQ as Q
import qualified Data.HashSet as S
import Data.Massiv.Array (Matrix, U, Ix2(..))
import qualified Data.Massiv.Array as A
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import OrphanInstances ()
import Utils (Grid, loadGrid)

data Maze = Maze {
    mazeSpaces :: Matrix U Bool, mazeStart :: Ix2, mazeEnd :: Ix2
} deriving Show

toMaze :: Grid -> Maze
toMaze grid = Maze{..}
  where
    mazeSpaces = A.compute $ A.map (/= '#') grid
    mazeStart = fromMaybe (error "No S") $ A.findIndex (== 'S') grid
    mazeEnd = fromMaybe (error "No E") $ A.findIndex (== 'E') grid

data SearchState = SS { ssPos :: !Ix2, ssDir :: !Ix2 }
    deriving (Eq, Ord, Show, Generic)

instance Hashable SearchState

heuristic :: Ix2 -> SearchState -> Int
heuristic (iEnd :. jEnd) (SS (i :. j) (di :. dj)) =
    dist + 1000 * max (turn i iEnd di) (turn j jEnd dj)
  where
    dist = abs (i - iEnd) + abs (j - jEnd)
    turn x xEnd dx
        | x == xEnd = 0                -- already arrived
        | dx == signum (xEnd - x) = 0  -- moving in right direction
        | dx == 0 = 1                  -- moving in perpendicular direction
        | otherwise = 2                -- moving in reverse direction

getMoves :: Maze -> SearchState -> [(Int, SearchState)]
getMoves Maze{..} SS{..} = forward ++ turnL ++ turnR
  where
    fwPos = ssPos + ssDir
    rdir = case ssDir of i :. j -> j :. -i

    forward = guard (mazeSpaces A.! fwPos) *> pure (1, SS fwPos ssDir)
    turnL = guard (mazeSpaces A.! (ssPos - rdir))
         *> pure (1000, SS ssPos (-rdir))
    turnR = guard (mazeSpaces A.! (ssPos + rdir))
         *> pure (1000, SS ssPos rdir)

search :: Maze -> Int
search maze@Maze{..} =
    go S.empty $ Q.singleton startState (heuristic mazeEnd startState) 0
  where
    startState = SS mazeStart (0 :. 1)

    go _ (Q.minView -> Nothing) = error "No path found"
    go visited (Q.minView -> Just (state, _, costSoFar, queue'))
      | ssPos state == mazeEnd = costSoFar
      | otherwise = go (S.insert state visited) $
                    foldl' (enqueue visited costSoFar) queue' $
                    getMoves maze state

    enqueue visited costSoFar queue (cost, ss)
      | ss `S.member` visited = queue
      | otherwise =
        let score = costSoFar + cost + heuristic mazeEnd ss
        in  case Q.lookup ss queue of
                Just (p, _) | p <= score -> queue
                _ -> Q.insert ss score (costSoFar + cost) queue

data FullSearchState = FS SearchState | FSEnd
    deriving (Eq, Ord, Show, Generic)

instance Hashable FullSearchState

getMovesF :: Maze -> SearchState -> [(Int, FullSearchState)]
getMovesF maze@Maze{..} ss
  | ssPos ss == mazeEnd = [(0, FSEnd)]
  | otherwise = [(cost, FS ss') | (cost, ss') <- getMoves maze ss]

searchF :: Maze -> S.HashSet Ix2
searchF maze@Maze{..} =
    go S.empty                -- result accumulator
       M.empty                -- best scores per SS
       (Q.singleton startState 0 S.empty)
  where
    startState = FS $ SS mazeStart (0 :. 1)

    go :: S.HashSet Ix2
       -> M.HashMap FullSearchState Int
       -> Q.HashPSQ FullSearchState Int (S.HashSet Ix2)
       -> S.HashSet Ix2
    go _ _ (Q.minView -> Nothing) = error "No path found"
    go _ _ (Q.minView -> Just (FSEnd, _, prevs, _)) = prevs
    go !res knownCosts (Q.minView -> Just (FS ss, costSoFar, prevs, queue')) =
        go res (M.insert (FS ss) costSoFar knownCosts) $
        foldl' (enqueue knownCosts costSoFar prevs) queue' $
        getMovesF maze ss

    enqueue knownCosts costSoFar prevs queue (cost, fss)
      | Just kc <- M.lookup fss knownCosts, total > kc =
        queue
      | otherwise =
        let (c, oldPrevs) = fromMaybe (maxBound, S.empty) (Q.lookup fss queue)
            prevs' = case fss of
                FS ss -> S.insert (ssPos ss) prevs
                FSEnd -> prevs
        in  case compare total c of
                LT -> Q.insert fss total prevs' queue
                EQ -> Q.insert fss total (S.union prevs' oldPrevs) queue
                GT -> queue
      where
        total = costSoFar + cost

main :: IO ()
main = do
    maze <- toMaze <$> loadGrid 16
    print $ search maze
    print $ S.size $ searchF maze
