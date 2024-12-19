module Main (main) where

import qualified Data.Text as T

import Utils (Text, loadInput, readInt)

parseLines :: Text -> [[Int]]
parseLines = map split . T.lines
  where
    split line = map readInt $ T.splitOn " " line

safeLevels :: [Int] -> Bool
safeLevels [] = True
safeLevels (x : xs) = all incr ds || all incr (map negate ds)
  where
    ds = zipWith (-) xs (x : xs)
    incr d = d >= 1 && d <= 3

dampen :: [a] -> [[a]]
dampen [] = [[]]
dampen (x : xs) = xs : map (x : ) (dampen xs)

safeLevelsDampened :: [Int] -> Bool
safeLevelsDampened = any safeLevels . dampen

main :: IO ()
main = do
    input <- parseLines <$> loadInput 2
    print $ length $ filter safeLevels input
    print $ length $ filter safeLevelsDampened input