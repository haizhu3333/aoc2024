module Main (main) where

import Data.MemoTrie (memoFix)
import qualified Data.Text as T

import Utils (Text, loadInput, readInt)

splitInput :: Text -> [Int]
splitInput = map readInt . T.splitOn " " . T.strip

digits :: Int -> Int
digits = go 0
  where
    go !d 0 = d
    go !d x = go (d + 1) (x `div` 10)

stonesF :: (Int -> Int) -> Int -> Int
stonesF recur 0 = recur 1
stonesF recur n
  | even d = recur q + recur r
  | otherwise = recur (n * 2024)
  where
    d = digits n
    (q, r) = divMod n (10 ^ (d `div` 2))

stones :: Int -> Int -> Int
stones = curry $ memoFix $ \recur (steps, n) ->
    if steps == 0
    then 1 
    else stonesF (\x -> recur (steps - 1, x)) n

allStones :: Int -> [Int] -> Int
allStones steps = sum . map (stones steps)

main :: IO ()
main = do
    txt <- loadInput 11
    let nums = splitInput txt
    print $ allStones 25 nums
    print $ allStones 75 nums
    print $ (maxBound :: Int)