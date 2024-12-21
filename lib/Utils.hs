module Utils (
    Parser, Grid, ByteString, Text,
    loadInput, readInt, parseInput, gridP, loadGrid
) where

import Paths_aoc2024 (getDataFileName)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Massiv.Array (Matrix, P)
import qualified Data.Massiv.Array as A
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Void (Void)
import Data.Word (Word8)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Printf (printf)

loadInputBytes :: Int -> IO ByteString
loadInputBytes day = do
    fileName <- getDataFileName $ printf "inputs/day%02d.txt" day
    B.readFile fileName

loadInput :: Int -> IO Text
loadInput day = T.decodeUtf8 <$> loadInputBytes day

readInt :: Integral a => Text -> a
readInt t = case T.decimal t of
    Left err -> error $ "Cannot parse int: " ++ show err
    Right (x, "") -> x
    Right (_, leftover) -> error $ "Incomplete parse: " ++ show leftover

type Parser = P.Parsec Void Text

parseInput :: Int -> Parser a -> IO a
parseInput day p = do
    txt <- loadInput day
    case P.parse (p <* P.eof) (printf "%02d.txt" day) txt of
        Left err -> do
            hPutStrLn stderr $ P.errorBundlePretty err
            exitFailure
        Right x -> return x

type Grid = Matrix P Char

gridP :: Parser Grid
gridP = do
    let line = P.takeWhile1P (Just "grid line") (/= '\n')
        row = A.map chr8 . A.castFromByteString A.Seq . T.encodeUtf8 <$> line
    rows <- row `P.sepEndBy1` P.newline
    case A.stackOuterSlicesM rows of
        Left err -> fail $ "Cannot stack array rows: " ++ show err
        Right arr -> pure $ A.computeS arr
  where       
    chr8 :: Word8 -> Char
    chr8 = toEnum . fromIntegral

loadGrid :: Int -> IO Grid
loadGrid day = parseInput day gridP
