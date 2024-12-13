module Utils (
    Parser, ByteString, Text, Grid, Word8,
    loadInputBytes, loadInput, parseInput, loadGrid, chr8
) where

import Paths_aoc2024 (getDataFileName)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Massiv.Array (Array, P, Ix2)
import qualified Data.Massiv.Array as A
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Void (Void)
import Data.Word (Word8)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (Parsec, parse, errorBundlePretty, eof)
import Text.Printf (printf)

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
    case parse (p <* eof) (printf "%02d.txt" day) txt of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right x -> return x

type Grid = Array P Ix2 Word8

loadGrid :: Int -> IO Grid
loadGrid day = do
    bstr <- loadInputBytes day
    arr <- A.stackOuterSlicesM [
        A.castFromByteString A.Seq line | line <- B.lines bstr]
    return $ A.computeS arr

chr8 :: Word8 -> Char
chr8 = toEnum . fromIntegral