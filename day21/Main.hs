{-# LANGUAGE StrictData, DeriveAnyClass #-}
module Main (main) where

import Data.Char (isDigit)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashPSQ as Q
import qualified Data.Text as T
import GHC.Generics (Generic)

import Utils (Text, loadInput, readInt)
import Vec (V2(..))

layoutD :: Char -> V2 Int
layoutD '^' = V2 0 1
layoutD 'v' = V2 1 1
layoutD '<' = V2 1 0
layoutD '>' = V2 1 2
layoutD 'A' = V2 0 2
layoutD x = error $ "Not on D pad: " ++ show x

layoutN :: Char -> V2 Int
layoutN '1' = V2 2 0
layoutN '2' = V2 2 1
layoutN '3' = V2 2 2
layoutN '4' = V2 1 0
layoutN '5' = V2 1 1
layoutN '6' = V2 1 2
layoutN '7' = V2 0 0
layoutN '8' = V2 0 1
layoutN '9' = V2 0 2
layoutN '0' = V2 3 1
layoutN 'A' = V2 3 2
layoutN x = error $ "Not on N pad: " ++ show x

dirs :: [(Char, V2 Int)]
dirs = [ ('^', V2 (-1) 0)
       , ('v', V2 1 0)
       , ('<', V2 0 (-1))
       , ('>', V2 0 1)
       ]

hasButtonD :: V2 Int -> Bool
hasButtonD (V2 0 j) = j == 1 || j == 2
hasButtonD (V2 1 j) = j >= 0 && j <= 2
hasButtonD _ = False

hasButtonN :: V2 Int -> Bool
hasButtonN (V2 i j) =
    i >= 0 && i <= 3 && j >= 0 && j <= 2 && not (i == 3 && j == 0)

buttonsD :: [Char]
buttonsD = "^v<>A"

buttonsN :: [Char]
buttonsN = "1234567890A"

data PadPos = PadPos (V2 Int) (V2 Int) | Done
    deriving (Eq, Ord, Show, Generic, Hashable)

addIfBetter :: (Hashable k, Ord k, Ord p)
            => k -> p -> Q.HashPSQ k p () -> Q.HashPSQ k p ()
addIfBetter k p queue = case Q.lookup k queue of
    Just (oldP, _) | oldP <= p -> queue
    _ -> Q.insert k p () queue

numSteps :: (V2 Int -> Bool)
         -> (V2 Int -> V2 Int -> Int)
         -> V2 Int -> V2 Int -> Int
numSteps hasButton costFn from to =
    go $ Q.singleton (PadPos from (layoutD 'A')) 0 ()
  where
    go (Q.minView -> Nothing) = error $ "Path not found " ++ show from ++ " -> " ++ show to
    go (Q.minView -> Just (Done, cost, _, _)) = cost
    go (Q.minView -> Just (PadPos pos dpos, cost, _, queue))
      | pos == to =
        go $ addDone dpos cost queue
      | otherwise =
        go $ foldl' (add pos dpos cost) queue dirs

    addDone dpos cost queue =
        let cost' = cost + costFn dpos (layoutD 'A')
        in  addIfBetter Done cost' queue

    add pos dpos cost queue (cmd, dir) =
        let pos' = pos + dir
            dpos' = layoutD cmd
            cost' = cost + costFn dpos dpos'
        in  if hasButton pos'
            then addIfBetter (PadPos pos' dpos') cost' queue
            else queue

type Table = HashMap (Int, V2 Int, V2 Int) Int

mkTable :: Int -> Table
mkTable depth = table
  where
    table = M.fromList $ do
        lv <- [0 .. depth]
        let fullLayout = if lv == 0
                         then layoutN <$> buttonsN
                         else layoutD <$> buttonsD
        a <- fullLayout
        b <- fullLayout
        pure ((lv, a, b), calc lv a b)

    calc lv =
        let hasButton = if lv == 0 then hasButtonN else hasButtonD
            costFn a b = if lv == depth then 1 else table M.! (lv + 1, a, b)
        in  numSteps hasButton costFn

wordSteps :: Table -> Text -> Int
wordSteps table word = sum
    [ table M.! (0, layoutN a, layoutN b)
    | (a, b) <- T.zip (T.cons 'A' word) word
    ]

scoreAll :: Int -> Text -> Int
scoreAll depth = sum . map score . T.lines
  where
    table = mkTable depth
    score w = readInt (T.filter isDigit w) * wordSteps table w

main :: IO ()
main = do
    text <- loadInput 21
    print $ scoreAll 2 text
    print $ scoreAll 25 text
