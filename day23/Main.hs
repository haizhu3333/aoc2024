module Main (main) where

import Control.Monad (guard)
import Data.Foldable (minimumBy, maximumBy)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (sort)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Utils (Text, loadInput)

type Graph = HashMap Text (HashSet Text)

getPair :: Text -> (Text, Text)
getPair t = case T.splitOn "-" t of
    [a, b] -> (a, b)
    _ -> error $ "Cannot parse: " ++ show t

addEdge :: Graph -> (Text, Text) -> Graph
addEdge g (a, b) = M.insertWith S.union a (S.singleton b)
                 $ M.insertWith S.union b (S.singleton a) g

mkGraph :: Text -> Graph
mkGraph = foldl' addEdge M.empty . map getPair . T.lines

triangles :: Graph -> [(Text, Text, Text)]
triangles g = do
    (a, fromA) <- M.toList g
    b <- S.toList fromA
    guard $ a < b
    c <- S.toList (g M.! b)
    guard $ b < c
    guard $ c `S.member` fromA
    pure (a, b, c)

trianglesWithT :: Graph -> Int
trianglesWithT = length . filter hasT . triangles
  where
    hasT (a, b, c) = any ("t" `T.isPrefixOf`) [a, b, c]

-- Bron-Kerbosch algorithm:
-- https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm#With_pivoting
maximalCliques :: Graph -> [HashSet Text]
maximalCliques graph = bronKerbosch2 S.empty (M.keysSet graph) S.empty
  where
    bronKerbosch2 r p x
      | S.null p && S.null x = [r]
      | otherwise = bkLoop r p x (S.toList $ pivot p x)

    bkLoop _ _ _ [] = []
    bkLoop r p x (v : vs) =
        let fromV = graph M.! v
            recur = bronKerbosch2 (S.insert v r)
                                  (p `S.intersection` fromV)
                                  (x `S.intersection` fromV)
        in recur ++ bkLoop r (S.delete v p) (S.insert v x) vs

    pivot p x = minimumBy (comparing S.size) $
        [p `S.difference` (graph M.! u) | u <- S.toList (p `S.union` x)]

maximumClique :: Graph -> Text
maximumClique = T.intercalate ","
              . sort
              . S.toList
              . maximumBy (comparing S.size)
              . maximalCliques

main :: IO ()
main = do
    graph <- mkGraph <$> loadInput 23
    print $ trianglesWithT graph
    T.putStrLn $ maximumClique graph
