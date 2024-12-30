{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Data.Bits ((.&.), shift, xor)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.Massiv.Array as A
import qualified Data.Text as T
import GHC.Generics (Generic)

import Utils (Text, loadInput, readInt)

next :: Int -> Int
next = mixPrune 11 . mixPrune (-5) . mixPrune 6
  where
    mixPrune sh x = (x `xor` (x `shift` sh)) .&. 0xFFFFFF

getSeeds :: Text -> [Int]
getSeeds = map readInt . T.lines

sum2000th :: [Int] -> Int
sum2000th = sum . map (fromIntegral . (!! 2000) . iterate next)

data DeltaSeq = DeltaSeq !Int !Int !Int !Int
    deriving (Eq, Ord, Show, Generic, Hashable)

newtype DeltaMap = DM (HashMap DeltaSeq Int)
instance Semigroup DeltaMap where
  (DM m1) <> (DM m2) = DM (M.unionWith (+) m1 m2)
instance Monoid DeltaMap where
  mempty = DM M.empty

mkDeltaSeq :: Int -> [(DeltaSeq, Int)]
mkDeltaSeq x0 = go 4 x4 (DeltaSeq (d x0 x1) (d x1 x2) (d x2 x3) (d x3 x4))
  where
    x1 = next x0
    x2 = next x1
    x3 = next x2
    x4 = next x3
    d x y = mod y 10 - mod x 10

    go !count _ _ | count > 2000 = []
    go !count !x ds@(DeltaSeq _ d2 d3 d4) =
        let x' = next x
        in  (ds, mod x 10) : go (count + 1) x' (DeltaSeq d2 d3 d4 (d x x'))

mkDeltaMap :: Int -> DeltaMap
mkDeltaMap = DM . foldl' add M.empty . mkDeltaSeq
  where
    add m (ds, value) = case M.lookup ds m of
        Nothing -> M.insert ds value m
        Just _ -> m

mkDeltaMapAll :: [Int] -> DeltaMap
mkDeltaMapAll = A.fold . A.map mkDeltaMap . A.fromList @A.P A.Par

getMaxDM :: DeltaMap -> Int
getMaxDM (DM m) = maximum m

main :: IO ()
main = do
    seeds <- getSeeds <$> loadInput 22
    print $ sum2000th seeds
    print $ getMaxDM $ mkDeltaMapAll seeds
