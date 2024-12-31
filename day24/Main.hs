module Main (main) where

import Control.Monad (guard, when)
import Data.Char (isAlphaNum)
import Data.Graph (stronglyConnComp, SCC (..))
import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe, isJust)
import Data.List (sortBy, sort)
import Data.Ord (comparing, Down(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Printf (printf)

import Utils (Parser, Text, parseInput)

type Name = Text
data Op = AND | OR | XOR deriving Show
data InitRule = InitRule Name Bool deriving Show
data CalcRule = CalcRule Name Op Name Name deriving Show
data Rules = Rules [InitRule] [CalcRule] deriving Show

nameP :: Parser Text
nameP = P.takeWhile1P (Just "wire name") isAlphaNum

initRulesP :: Parser [InitRule]
initRulesP = ruleP `P.sepEndBy` P.newline
  where
    ruleP = InitRule <$> nameP <* ": " <*> value
    value = P.choice [True <$ "1", False <$ "0"]

calcRulesP :: Parser [CalcRule]
calcRulesP = ruleP `P.sepEndBy` P.newline
  where
    ruleP = CalcRule <$> nameP <* " "
                     <*> op <* " "
                     <*> nameP <* " -> "
                     <*> nameP
    op = P.choice [AND <$ "AND", OR <$ "OR", XOR <$ "XOR"]

rulesP :: Parser Rules
rulesP = Rules <$> initRulesP <* P.newline <*> calcRulesP

mkWireMap :: Rules -> ([Name], Name -> Maybe Bool)
mkWireMap (Rules irs crs) = (assigned, get)
  where
    assigned = M.keys $ M.filterWithKey (const isJust) crMap
    combined = M.union irMap crMap
    get name = fromMaybe Nothing $ M.lookup name combined
    irMap = M.fromList [(name, Just value) | InitRule name value <- irs]
    crMap = M.fromList
        [ (out, eval op (get lhs) (get rhs))
        | CalcRule lhs op rhs out <- crs
        ]
    eval AND = liftA2 (&&)
    eval OR = liftA2 (||)
    eval XOR = liftA2 (/=)

extractInt :: Text -> [Name] -> (Name -> Maybe Bool) -> Int
extractInt prefix names get =
    foldl' bit 0 .
    sortBy (comparing $ Down . fst) $
    [(name, get name) | name <- names, prefix `T.isPrefixOf` name]
  where
    bit x (_, Just b) = x * 2 + fromEnum b
    bit _ (name, Nothing) = error $ "Missing value " ++ show name

-- Mega hacky part 2!
-- Basic idea as follows:
-- We go up from bit 0 to bit 44. For each bit we simulate the circuit using
-- only 3 inputs assigned: xNN, yNN (NN = the bit number) and the carry wire
-- that was found from the previous step. For bit 0 we use a dummy carry which
-- is fixed to 0.
-- Each simulation is done 8 times using all combinations of x, y, and carry.
-- If one of the wires consistently produces an output that is equal to
--    X xor Y xor C, that is determined to be the output wire; or
--    X&&Y || (C&&(X||Y)), that is the carry wire.
-- If there is an output wire found and its name is zNN, and an carry-out found,
-- move on to the next bit.
-- Otherwise attempt to fix it by doing some swaps, as described in the function
-- `corrections'.

findWiresGeneric :: (Bool -> Bool -> Bool -> Bool)
                 -> [CalcRule] -> Int -> Maybe Name -> ([Name], [Name])
findWiresGeneric f crs bitNum carry = (wires, affected)
  where
    wires = [ name | CalcRule _ _ _ name <- crs, all ($ name) checkers ]

    goldens = do
        xVal <- [True, False]
        yVal <- [True, False]
        cVal <- case carry of
            Nothing -> [False]
            Just _  -> [True, False]
        let xIR = InitRule (knownName 'x' bitNum) xVal
            yIR = InitRule (knownName 'y' bitNum) yVal
            cIR = InitRule (fromMaybe "_dummy" carry) cVal
            (assigned, get) = mkWireMap (Rules [xIR, yIR, cIR] crs)
        pure (assigned, get, f xVal yVal cVal)

    checkers = do
        (_, get, expected) <- goldens
        pure $ \name -> get name == Just expected

    affected = case goldens of
        (aff, _, _) : _ -> aff
        _ -> error "Impossible! goldens is empty"

knownName :: Char -> Int -> Name
knownName prefix bitNum = T.pack $ printf "%c%02d" prefix bitNum

findOut :: [CalcRule] -> Int -> Maybe Name -> ([Name], [Name])
findOut = findWiresGeneric $ \a b c -> (a /= b) /= c

findCarry :: [CalcRule] -> Int -> Maybe Name -> ([Name], [Name])
findCarry = findWiresGeneric $ \a b c -> a && b || ((a || b) && c)

swapCRs :: Name -> Name -> [CalcRule] -> [CalcRule]
swapCRs n1 n2 = map swap
  where
    swap (CalcRule a op b r) | r == n1 = CalcRule a op b n2
    swap (CalcRule a op b r) | r == n2 = CalcRule a op b n1
    swap cr = cr

hasCycle :: [CalcRule] -> Bool
hasCycle crs = any isCyclic sccs
  where
    sccs = stronglyConnComp $
        [(node, node, edges) | (node, edges) <- M.toList adjList]

    adjList = M.fromListWith (++) $ do
        CalcRule a _ b r <- crs
        [(a, [r]), (b, [r])]

    isCyclic (NECyclicSCC _) = True
    isCyclic _ = False

isValidOut :: Int -> ([Name], [Name]) -> Bool
isValidOut bitNum ([o], _) | o == knownName 'z' bitNum = True
isValidOut _ _ = False

findSwapOut :: Int -> Maybe Name -> [CalcRule] -> [Name]
            -> [(Name, Name, [CalcRule])]
findSwapOut bitNum carry crs affected = do
    a <- affected
    b <- affected
    guard $ a < b
    let crs' = swapCRs a b crs
    guard $ not (hasCycle crs') && isValidOut bitNum (findOut crs' bitNum carry)
    pure (a, b, crs')

-- Only implemented corrections that are necessary for my particular input.
-- We find all wires that are involved (those that can be assigned a value
-- starting from only xNN, yNN and previous carry), and try to swap them to see
-- if that then produces a valid output wire.
-- Care must be taken to check for cycles are introduced in the graph by the
-- swap, as `mkWireMap' will infinite loop if there are cycles.
-- If the above doesn't work, print out the current situation and stop.
corrections :: [CalcRule] -> IO [Name]
corrections initCRs = checkOut [] initCRs 0 Nothing
  where
    checkOut :: [Name] -> [CalcRule] -> Int -> Maybe Name -> IO [Name]
    checkOut swaps crs bitNum carry = case findOut crs bitNum carry of
        result | isValidOut bitNum result ->
            checkCarry swaps crs bitNum carry
        (os, affected) -> case findSwapOut bitNum carry crs affected of
            (a, b, crs') : _ -> do
                printf "Swapping %s with %s\n" a b
                checkCarry (a : b : swaps) crs' bitNum carry
            [] -> do
                printf "Bit %d carry %s: found out=%s\n  affected=%s\n"
                       bitNum (show carry) (show os) (show affected)
                pure swaps

    checkCarry :: [Name] -> [CalcRule] -> Int -> Maybe Name -> IO [Name]
    checkCarry swaps crs bitNum carry = case findCarry crs bitNum carry of
        ([o], _) ->
            checkOut swaps crs (bitNum + 1) (Just o)
        (os, affected) -> do
            printf "Bit %d carry %s: found carry=%s\n  affected=%s\n"
                bitNum (show carry) (show os) (show affected)
            pure swaps

main :: IO ()
main = do
    rules@(Rules _ crs) <- parseInput 24 rulesP
    let (names, get) = mkWireMap rules
    print $ extractInt "z" names get
    swaps <- corrections crs
    when (length swaps == 8) $
        T.putStrLn $ T.intercalate "," $ sort swaps
