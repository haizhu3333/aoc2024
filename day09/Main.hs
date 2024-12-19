{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Control.Monad.ST (ST)
import Data.Char (isDigit, digitToInt)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (intersperse, minimumBy)
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as MU

import Utils (Text, loadInput)
import Control.Monad (guard)
import Data.Function (on)

convertInput :: Text -> [Int]
convertInput txt = map digitToInt $ T.unpack $ T.filter isDigit txt

empty :: Int
empty = -1

makeBlocks :: [Int] -> Vector Int
makeBlocks xs =
    U.concat [U.replicate n label | (label, n) <- labelInput xs]
  where
    labelInput = zip (intersperse empty [0 ..])

defragImpl :: STVector s Int -> Int -> Int -> ST s ()
defragImpl v l r
  | l >= r = pure ()
  | otherwise = do
    lVal <- MU.read v l
    rVal <- MU.read v r
    if | lVal /= empty -> defragImpl v (l + 1) r
       | rVal == empty -> defragImpl v l (r - 1)
       | otherwise -> do
          MU.write v l rVal
          MU.write v r empty
          defragImpl v (l + 1) (r - 1)

defrag :: Vector Int -> Vector Int
defrag = U.modify $ \mv -> defragImpl mv 0 (MU.length mv - 1)

checksumVector :: Vector Int -> Int
checksumVector = U.sum . U.map c . U.indexed
  where
    c (i, label) | label == empty = 0
                 | otherwise = i * label

solve1 :: Text -> Int
solve1 = checksumVector . defrag . makeBlocks . convertInput

type Files = IntMap (Int, Int)  -- maps file id to position x length
type FreeBlocks = IntMap IntSet  -- maps length to set of positions

filesAndFreeBlocks :: [Int] -> (Files, FreeBlocks)
filesAndFreeBlocks = onFile IM.empty IM.empty 0 0
  where
    onFile files frees _ _ [] = (files, frees)
    onFile files frees fid pos (x : xs) =
        let files' = IM.insert fid (pos, x) files
        in  onFree files' frees (fid + 1) (pos + x) xs

    onFree files frees _ _ [] = (files, frees)
    onFree files frees fid pos (x : xs) =
        let frees' = IM.insertWith IS.union x (IS.singleton pos) frees
        in  onFile files frees' fid (pos + x) xs

safeMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMinimumBy _ [] = Nothing
safeMinimumBy f xs = Just $ minimumBy f xs

findFirstFree :: FreeBlocks -> Int -> Int -> Maybe (Int, Int)
findFirstFree frees currPos size = safeMinimumBy (compare `on` fst) $ do
    (freeLen, freeSet) <- IM.toList frees
    guard $ freeLen >= size
    guard $ not $ IS.null freeSet
    let left = IS.findMin freeSet
    guard $ left <= currPos - size
    pure (left, freeLen)

moveFile :: Files -> FreeBlocks -> Int -> Int -> Maybe (Int, Int)
         -> (Files, FreeBlocks)
moveFile files frees _ _ Nothing = (files, frees)
moveFile files frees fid size (Just (to, freeLen)) = (files', frees')
  where
    files' = IM.insert fid (to, size) files
    frees' = IM.adjust (IS.insert (to + size)) (freeLen - size) $
             IM.adjust (IS.delete to) freeLen frees

defragFiles :: (Files, FreeBlocks) -> (Files, FreeBlocks)
defragFiles (files0, frees0) =
    foldl' go (files0, frees0) (IM.toDescList files0)
  where
    go (files, frees) (fid, (pos, size)) =
        let ff = findFirstFree frees pos size
        in  moveFile files frees fid size ff

checksumFiles :: Files -> Int
checksumFiles = sum . map c . IM.toList
  where
    c (fid, (pos, size)) = fid * sum [pos .. pos + size - 1]

solve2 :: Text -> Int
solve2 = checksumFiles . fst . defragFiles . filesAndFreeBlocks . convertInput

main :: IO ()
main = do
    txt <- loadInput 9
    print $ solve1 txt
    print $ solve2 txt