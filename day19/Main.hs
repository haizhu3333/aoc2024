{-# LANGUAGE StrictData #-}
module Main (main) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify')
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Char (isLower)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Utils (Parser, Text, parseInput)

data Input = Input { pieces :: [Text], strings :: [Text] }
    deriving Show

inputP :: Parser Input
inputP = do
    let word = P.takeWhile1P (Just "word") isLower
    pieces <- word `P.sepBy` ", "
    _ <- P.newline *> P.newline
    strings <- word `P.sepEndBy` P.newline
    pure Input{..}

type MemoTable = HashMap Text Int
type T m = StateT MemoTable (ReaderT [Text] m)

numParses :: Monad m => Text -> T m Int
numParses "" = pure 1
numParses text = do
    result <- gets (M.lookup text)
    case result of
        Just n -> pure n
        Nothing -> do
            n <- calcNumParses text
            modify' $ M.insert text n
            pure n

calcNumParses :: Monad m => Text -> T m Int
calcNumParses text = do
    ws <- lift ask
    counts <- forM ws $ \w -> do
        case T.stripPrefix w text of
            Nothing -> pure 0
            Just text' -> numParses text'
    pure $ sum counts

totalParseable :: Monad m => [Text] -> T m Int
totalParseable = fmap (length . filter (> 0)) . mapM numParses

totalNumParses :: Monad m => [Text] -> T m Int
totalNumParses = fmap sum . mapM numParses

main :: IO ()
main = do
    Input{..} <- parseInput 19 inputP
    flip runReaderT pieces $ flip evalStateT M.empty $ do
        totalParseable strings >>= liftIO . print
        totalNumParses strings >>= liftIO . print
