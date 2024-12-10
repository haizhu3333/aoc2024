module Utils (Parser, ByteString, Text, loadInputBytes, loadInput, parseInput) where

import Paths_aoc2024 (getDataFileName)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import Text.Printf (printf)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

loadInputBytes :: Int -> IO ByteString
loadInputBytes day = do
    fileName <- getDataFileName $ printf "inputs/day%02d.txt" day
    B.readFile fileName

loadInput :: Int -> IO Text
loadInput day = T.decodeUtf8 <$> loadInputBytes day

type Parser = Parsec Void Text

parseInput :: Int -> Parser a -> IO a
parseInput day p = do
    txt <- loadInput day
    case parse p (printf "%02d.txt" day) txt of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right x -> return x
