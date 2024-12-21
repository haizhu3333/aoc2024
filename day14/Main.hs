module Main (main) where

import qualified Data.HashSet as S

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Utils (Parser, parseInput)
import Vec (V2(..))
import Control.Monad (forM_)
import Data.List (minimumBy)
import Data.Function (on)
import Text.Printf (printf)

data Bot = Bot { botPos :: V2 Int, botVel :: V2 Int }
    deriving Show

type Size = V2 Int

signedDecimal :: Parser Int
signedDecimal = P.signed (pure ()) P.decimal

botP :: Parser Bot
botP = do
    _ <- P.string "p="
    px <- P.decimal
    _ <- P.char ','
    py <- P.decimal
    _ <- P.string " v="
    vx <- signedDecimal
    _ <- P.char ','
    vy <- signedDecimal
    pure $ Bot (V2 px py) (V2 vx vy)

botsP :: Parser [Bot]
botsP = botP `P.sepEndBy` P.newline

roomSize :: Size
roomSize = V2 101 103

step :: Size -> Bot -> Bot
step sz (Bot p v) = Bot p' v
  where
    p' = liftA2 mod (p + v) sz

stepAll :: Size -> Int -> [Bot] -> [Bot]
stepAll sz time = map $ \bot -> iterate (step sz) bot !! time

data QuadCount = QuadCount !Int !Int !Int !Int deriving Show

addQC :: Size -> QuadCount -> V2 Int -> QuadCount
addQC (V2 width height) (QuadCount a b c d) (V2 x y) =
    case (compare x xmid, compare y ymid) of
    (LT, LT) -> QuadCount (a + 1) b c d
    (LT, GT) -> QuadCount a (b + 1) c d
    (GT, LT) -> QuadCount a b (c + 1) d
    (GT, GT) -> QuadCount a b c (d + 1)
    _ -> QuadCount a b c d
  where
    xmid = width `div` 2
    ymid = height `div` 2

productQC :: Size -> [Bot] -> Int
productQC sz bots =
    case foldl' (addQC sz) (QuadCount 0 0 0 0) (map botPos bots) of
        QuadCount a b c d -> a * b * c * d

printBots :: Size -> [Bot] -> IO ()
printBots (V2 width height) bots = do
    forM_ [0 .. height - 1] $ \y -> do
        forM_ [0 .. width - 1] $ \x -> do
            putChar (if (V2 x y) `S.member` posSet then '#' else ' ')
        putChar '\n'
  where
    posSet = S.fromList (map botPos bots)

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

scoreBots :: [Bot] -> Double
scoreBots bots = average (map dist2 poss)
  where
    poss = map (fmap fromIntegral . botPos) bots
    V2 muX muY = sum poss / fromIntegral (length poss)
    dist2 (V2 x y) = (x - muX) ^ 2 + (y - muY) ^ 2

data EEMeasure = EEMeasure { eeTime :: Int, eeBots :: [Bot], eeScore :: Double}
    deriving Show

minEEScore :: Size -> [Bot] -> EEMeasure
minEEScore sz@(V2 width height) bots =
    minimumBy (compare `on` eeScore) measures
  where
    maxTime = lcm width height
    measures = do
        (i, bots') <- take maxTime $ zip [0..] $ iterate (map (step sz)) bots
        pure EEMeasure{ eeTime = i, eeBots = bots', eeScore = scoreBots bots' }

main :: IO ()
main = do
    bots <- parseInput 14 botsP
    print $ productQC roomSize (stepAll roomSize 100 bots)
    let best = minEEScore roomSize bots
    printBots roomSize (eeBots best)
    printf "Time: %d | Score: %.6f\n" (eeTime best) (eeScore best)