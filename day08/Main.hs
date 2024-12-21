module Main (main) where

import Data.Char (isAlphaNum)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (tails)
import Data.Massiv.Array (Sz2, Ix2)
import qualified Data.Massiv.Array as A

import OrphanInstances ()
import Utils (Grid, loadGrid)

type FreqMap = M.HashMap Char [Ix2]
type Antinodes = S.HashSet Ix2
type Rule = Sz2 -> Ix2 -> Ix2 -> [Ix2]

toFreqMap :: Grid -> FreqMap
toFreqMap = A.ifoldlS go M.empty
  where
    go freqs pos c
      | isAlphaNum c = M.insertWith (++) c [pos] freqs
      | otherwise = freqs

antinodes :: Rule -> Sz2 -> [Ix2] -> Antinodes
antinodes rule size poslist = S.fromList $ do
    p0 : ps <- tails poslist
    p1 <- ps
    rule size p0 p1

allAntinodes :: Rule -> Grid -> Antinodes
allAntinodes rule grid =
    S.unions [antinodes rule size poslist | poslist <- M.elems freqs]
  where
    size = A.size grid
    freqs = toFreqMap grid

rule1 :: Rule
rule1 size p0 p1 = filter (A.isSafeIndex size) [p0 - dp, p1 + dp]
  where
    dp = p1 - p0

rule2 :: Rule
rule2 size p0 p1 = extend p1 dp ++ extend p0 (- dp)
  where
    dp = p1 - p0
    extend p d = takeWhile (A.isSafeIndex size) (iterate (+ d) p)

main :: IO ()
main = do
    grid <- loadGrid 8
    print $ S.size $ allAntinodes rule1 grid
    print $ S.size $ allAntinodes rule2 grid