module Main (main) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (sort)
import qualified Data.Text as T

import Utils (Text, loadInput, readInt)

parseTwoCols :: Text -> ([Int], [Int])
parseTwoCols = unzip . map toPair . T.lines
  where
    toPair line = case T.splitOn "   " line of
        [x, y] -> (readInt x, readInt y)
        _ -> error $ "Bad format " ++ show line

totalDist :: [Int] -> [Int] -> Int
totalDist xs ys = sum $ zipWith d (sort xs) (sort ys)
  where
    d x y = abs (x - y)

histogram :: [Int] -> HashMap Int Int
histogram xs = M.fromListWith (+) [(x, 1) | x <- xs]

similarity :: [Int] -> [Int] -> Int
similarity xs ys = sum [x * M.lookupDefault 0 x h | x <- xs]
  where
    h = histogram ys

main :: IO ()
main = do
    input <- loadInput 1
    let (xs, ys) = parseTwoCols input
    print $ totalDist xs ys
    print $ similarity xs ys