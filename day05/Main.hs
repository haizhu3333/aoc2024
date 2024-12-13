module Main (main) where

import Control.Monad.ST
import Data.STRef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (tails)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Utils (Parser, parseInput)

dependP :: Parser (Int, Int)
dependP = (,) <$> P.decimal <* P.char '|' <*> P.decimal

sequenceP :: Parser [Int]
sequenceP = P.decimal `P.sepBy1` P.char ','

data PrintData = PrintData {
    depends :: HashSet (Int, Int),
    sequences :: [[Int]]
} deriving Show

printDataP :: Parser PrintData
printDataP = do
    depends <- S.fromList <$> dependP `P.sepEndBy` P.newline
    _ <- P.newline
    sequences <- sequenceP `P.sepEndBy` P.newline
    pure PrintData{..}

isSeqOK :: HashSet (Int, Int) -> [Int] -> Bool
isSeqOK deps xs = and $ do
    x1 : tl <- tails xs
    x2 <- tl
    pure $ (x2, x1) `notElem` deps

midElem :: [a] -> a
midElem xs = xs !! (length xs `div` 2)

sumMidOfOKSeqs :: HashSet (Int, Int) -> [[Int]] -> Int
sumMidOfOKSeqs deps seqs =
    sum [midElem s | s <- seqs, isSeqOK deps s]

subgraph :: HashSet (Int, Int) -> [Int] -> HashMap Int [Int]
subgraph deps xs = S.foldl' add M.empty deps
  where
    add g (x, y)
      | x `elem` xset && y `elem` xset =
        M.insertWith (++) x [y] $
        M.insertWith (++) y [] g
      | otherwise = g
    xset = S.fromList xs

topoSort :: HashMap Int [Int] -> [Int]
topoSort g = runST $ do
    nodesRef <- newSTRef (M.map (const True) g)
    accRef <- newSTRef []
    loop nodesRef accRef
  where
    loop nodesRef accRef = do
        nodes <- readSTRef nodesRef
        case M.keys nodes of
            [] -> readSTRef accRef
            x : _ -> do
                visit nodesRef accRef x
                loop nodesRef accRef
    
    visit nodesRef accRef x = do
        nodes <- readSTRef nodesRef
        case M.lookup x nodes of
            Nothing -> pure ()
            Just False -> error $ "Cycle at " ++ show x
            Just True -> doVisit nodesRef accRef x
    
    doVisit nodesRef accRef x = do
        modifySTRef' nodesRef (M.insert x False)
        mapM_ (visit nodesRef accRef) (g M.! x)
        modifySTRef' nodesRef (M.delete x)
        modifySTRef' accRef (x :)

sumMidOfFixedSeqs :: HashSet (Int, Int) -> [[Int]] -> Int
sumMidOfFixedSeqs deps seqs =
    sum [midElem $ topoSort $ subgraph deps s
        | s <- seqs
        , not $ isSeqOK deps s
        ]

main :: IO ()
main = do
    PrintData deps seqs <- parseInput 5 printDataP
    print $ sumMidOfOKSeqs deps seqs
    print $ sumMidOfFixedSeqs deps seqs