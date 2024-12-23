{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Applicative (some, empty)
import Control.Monad (forM_, unless)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Massiv.Array (Array, U, Ix1, Ix2(..), MArray, MonadThrow)
import qualified Data.Massiv.Array as A
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import OrphanInstances ()
import Utils (Parser, Grid, gridP, parseInput)

data Cell = Empty | Box | Wall deriving (Eq, Show, Enum)
derivingUnbox "Cell"
    [t| Cell -> Int |]
    [| fromEnum |]
    [| toEnum |]

data Scenario = Scenario
    { cellGrid :: Array U Ix2 Cell
    , robotStart :: Ix2
    , instructions :: Array U Ix1 Ix2
    } deriving Show

toCell :: MonadFail m => Char -> m Cell
toCell '.' = pure Empty
toCell '@' = pure Empty
toCell 'O' = pure Box
toCell '#' = pure Wall
toCell ch = fail $ "Unexpected character " ++ show ch

instrP :: Parser Ix2
instrP = P.choice
    [ ( 0 :. -1) <$ P.char '<'
    , ( 0 :.  1) <$ P.char '>'
    , (-1 :.  0) <$ P.char '^'
    , ( 1 :.  0) <$ P.char 'v'
    ]

findStart :: MonadFail m => Grid -> m Ix2
findStart grid = case A.findIndex (== '@') grid of
    Nothing -> fail "No '@' found"
    Just pos -> pure pos

scenarioP :: Parser Scenario
scenarioP = do
    grid <- gridP
    _ <- P.newline
    instrLines <- some instrP `P.sepEndBy` P.newline
    cellGrid <- A.mapM @U toCell grid
    robotStart <- findStart grid
    let instructions = A.fromList A.Seq (concat instrLines)
    pure Scenario{..}

findNonBox :: MArray s U Ix2 Cell -> Ix2 -> Ix2 -> ST s (Ix2, Bool)
findNonBox cells pos dir = go (pos + dir)
  where
    go ix = do
        c <- A.readM cells ix
        case c of
            Empty -> pure (ix, True)
            Box -> go (ix + dir)
            Wall -> pure (ix, False)

execMove :: MArray s U Ix2 Cell -> Ix2 -> Ix2 -> ST s Ix2
execMove cells pos dir = do
    (target, canMove) <- findNonBox cells pos dir
    if canMove
    then do
        let next = pos + dir
        nextVal <- A.readM cells next
        A.writeM cells target nextVal
        A.writeM cells next Empty
        pure next
    else
        pure pos

runMoves :: Scenario -> Array U Ix2 Cell
runMoves Scenario{..} = A.withMArrayST_ cellGrid $ \cells ->
    A.foldlM_ (execMove cells) robotStart instructions

scoreBoxes :: Array U Ix2 Cell -> Int
scoreBoxes = A.sum . A.imap f
  where
    f (i :. j) Box = i * 100 + j
    f _ _ = 0

data CellEx = EmptyEx | BoxL | BoxR | WallEx deriving (Eq, Show, Enum)
derivingUnbox "CellEx"
    [t| CellEx -> Int |]
    [| fromEnum |]
    [| toEnum |]

expand :: MonadThrow m => Array U Ix2 Cell -> m (Array U Ix2 CellEx)
expand cells = do
    stacked <- A.stackSlicesM (A.Dim 1) [
        A.map (ex BoxL) cells, A.map (ex BoxR) cells]
    let A.Sz3 h s w = A.size stacked
    arr <- A.resizeM (A.Sz2 h (s * w)) stacked
    pure $ A.compute arr
  where
    ex _ Empty = EmptyEx
    ex boxEx Box = boxEx
    ex _ Wall = WallEx

pushableEx :: MArray s U Ix2 CellEx -> Ix2 -> Ix2
           -> ST s (Maybe (HashMap Ix2 CellEx))
pushableEx cells start dir = runMaybeT $ do
    result <- lift $ newSTRef M.empty
    go result start
    lift $ readSTRef result
  where
    go result pos = do
        seen <- lift $ readSTRef result
        let pos' = pos + dir
        unless (pos' `M.member` seen) $ go' result pos'
    go' result pos = do
        c <- A.readM cells pos
        boxes <- case c of
            EmptyEx -> pure []
            BoxL -> pure [(pos, BoxL), (pos + Ix2 0 1, BoxR)]
            BoxR -> pure [(pos, BoxR), (pos - Ix2 0 1, BoxL)]
            WallEx -> empty
        forM_ boxes $ \(ix, value) -> do
            lift $ modifySTRef' result (M.insert ix value)
            go result ix

execMoveEx :: MArray s U Ix2 CellEx -> Ix2 -> Ix2 -> ST s Ix2
execMoveEx cells pos dir = do
    pushable <- pushableEx cells pos dir
    case pushable of
        Nothing -> pure pos
        Just boxes -> do
            forM_ (M.keys boxes) $ \ix -> A.writeM cells ix EmptyEx
            forM_ (M.toList boxes) $ \(ix, val) -> A.writeM cells (ix + dir) val
            pure (pos + dir)

runMovesEx :: Scenario -> Array U Ix2 CellEx
runMovesEx Scenario{..} = runST $ do
    gridEx <- expand cellGrid
    A.withMArrayS_ gridEx $ \cells -> do
        let startEx = robotStart * (1 :. 2)
        A.foldlM_ (execMoveEx cells) startEx instructions

scoreBoxesEx :: Array U Ix2 CellEx -> Int
scoreBoxesEx = A.sum . A.imap f
  where
    f (i :. j) BoxL = i * 100 + j
    f _ _ = 0

main :: IO ()
main = do
    scenario <- parseInput 15 scenarioP
    print $ scoreBoxes $ runMoves scenario
    print $ scoreBoxesEx $ runMovesEx scenario