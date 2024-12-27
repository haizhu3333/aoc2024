module Main (main) where

import qualified Data.HashPSQ as Q
import qualified Data.HashSet as S
import Data.Maybe (isJust)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.Printf (printf)

import Utils (Parser, parseInput)
import Vec (V2(..))

pointsP :: Parser [V2 Int]
pointsP = pt `P.sepEndBy` P.newline
  where
    pt = V2 <$> P.decimal <* P.char ',' <*> P.decimal

dirs :: [V2 Int]
dirs = [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]

heuristic :: V2 Int -> V2 Int -> Int
heuristic (V2 i j) (V2 i' j') = abs (i - i') + abs (j - j')

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 ib jb) (V2 i j) = i >= 0 && j >= 0 && i <= ib && j <= jb

search :: V2 Int -> S.HashSet (V2 Int) -> Maybe Int
search endPt walls =
    go S.empty (Q.singleton start (heuristic endPt start) 0)
  where
    start = V2 0 0

    moves visited pt =
        [ pt + d 
        | d <- dirs
        , inBounds endPt pt
        , pt `notElem` visited
        , pt `notElem` walls
        ]

    go _ (Q.minView -> Nothing) = Nothing
    go visited (Q.minView -> Just (pt, _, cost, queue))
      | pt == endPt = pure cost
      | otherwise = go (S.insert pt visited)
                       (foldl' (add cost) queue (moves visited pt))

    add cost queue pt =
        let cost' = cost + 1 in
        case Q.lookup pt queue of
            Just (_, v) | v <= cost' -> queue
            _ -> Q.insert pt (cost' + heuristic endPt pt) cost' queue

firstBlock :: V2 Int -> [V2 Int] -> V2 Int
firstBlock endPt pts = pts !! bsearch 0 (length pts)
  where
    pathable n = isJust $ search endPt (S.fromList $ take n pts)

    bsearch nPath nBlock
      | nBlock - nPath <= 1 = nPath
      | otherwise =
        let mid = (nPath + nBlock) `div` 2 in
        if pathable mid
        then bsearch mid nBlock
        else bsearch nPath mid

printPt :: V2 Int -> IO ()
printPt (V2 i j) = printf "%d,%d\n" i j

main :: IO ()
main = do
    pointsList <- parseInput 18 pointsP
    let endPt = V2 70 70
    print $ search endPt (S.fromList $ take 1024 pointsList)
    printPt $ firstBlock endPt pointsList
