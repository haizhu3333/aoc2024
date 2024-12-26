module Main (main) where

import Control.Applicative ((<|>))
import Data.Bits (Bits(..))
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Streaming (Stream, Of, runIdentity)
import qualified Streaming.Prelude as S
import System.IO (hClose)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.Printf (printf)

import Utils (Parser, parseInput)

data VM = VM { ip :: !Int, ra :: !Int, rb :: !Int, rc :: !Int }
    deriving Show

regP :: Char -> Parser Int
regP ch = P.string "Register " *> P.char ch *> P.string ": "
       *> P.decimal <* P.newline

vmP :: Parser (VM, Vector Int)
vmP = do
    ra <- regP 'A'
    rb <- regP 'B'
    rc <- regP 'C'
    _ <- P.string "\nProgram: "
    code <- P.decimal `P.sepBy` P.char ',' <* P.newline
    pure (VM{ip = 0, ..}, V.fromList code)

halt :: Vector Int -> VM -> Bool
halt code VM{ip} = ip < 0 || ip >= V.length code

comboArg :: VM -> Int -> Int
comboArg VM{..} arg = case arg of
    _ | arg >= 0 && arg <= 3 -> arg
    4 -> ra
    5 -> rb
    6 -> rc
    _ -> error $ "Invalid combo arg " ++ show arg

step :: Monad m => Vector Int -> VM -> Stream (Of Int) m VM
step code vm = case instr of
    0 -> pure vm{ra = ra vm `shiftR` argC}
    1 -> pure vm{rb = rb vm `xor` argL}
    2 -> pure vm{rb = argC .&. 7}
    3 | ra vm == 0 -> pure vm
      | otherwise  -> pure vm{ip = argL - 2}
    4 -> pure vm{rb = rb vm `xor` rc vm}
    5 -> S.yield (argC .&. 7) *> pure vm
    6 -> pure vm{rb = ra vm `shiftR` argC}
    7 -> pure vm{rc = ra vm `shiftR` argC}
    _ -> error $ "Invalid opcode " ++ show instr
  where
    instr = code V.! ip vm
    argL = code V.! (ip vm + 1)
    argC = comboArg vm argL

run :: Monad m => Vector Int -> VM -> Stream (Of Int) m VM
run code vm
  | halt code vm = pure vm
  | otherwise = do
        vm' <- step code vm
        run code vm'{ip = ip vm' + 2}

getOutput :: Vector Int -> VM -> [Int]
getOutput code vm = runIdentity $ S.toList_ (run code vm)

data SM = SM {
    smIP :: !Int,
    smRA :: !String, smRB :: !String, smRC :: !String,
    smNextVar :: !Int, smExpectedOuts :: [Int]
} deriving Show

sLit :: Int -> String
sLit a = printf "(_ bv%d 64)" a

stepSM :: Monad m => Vector Int -> SM -> Stream (Of String) m SM
stepSM code sm = case instr of
    0 -> do S.yield $ printf "(define-const %s (_ BitVec 64) (bvlshr %s %s))"
                             var (smRA sm) argC
            pure smNewVar{smRA = var}
    1 -> do S.yield $ printf "(define-const %s (_ BitVec 64) (bvxor %s %s))"
                             var (smRB sm) argL
            pure smNewVar{smRB = var}
    2 -> do S.yield $ printf "(define-const %s (_ BitVec 64) (bvand %s %s))"
                             var argC (sLit 7)
            pure smNewVar{smRB = var}
    3 | null (smExpectedOuts sm) -> do
            S.yield $ printf "(assert (= %s %s))" (smRA sm) (sLit 0)
            pure sm
      | otherwise -> do
            S.yield $ printf "(assert (not (= %s %s)))" (smRA sm) (sLit 0)
            pure sm{smIP = arg - 2}            
    4 -> do S.yield $ printf "(define-const %s (_ BitVec 64) (bvxor %s %s))"
                             var (smRB sm) (smRC sm)
            pure smNewVar{smRB = var}
    5 -> do let (out, outs) = case smExpectedOuts sm of
                    [] -> error "Out of expected outputs"
                    x : xs -> (x, xs)
            S.yield $ printf "(assert (= (bvand %s %s) %s))"
                           argC (sLit 7) (sLit out)
            pure sm{smExpectedOuts = outs}
    6 -> do S.yield $ printf "(define-const %s (_ BitVec 64) (bvlshr %s %s))"
                             var (smRA sm) argC
            pure smNewVar{smRB = var}
    7 -> do S.yield $ printf "(define-const %s (_ BitVec 64) (bvlshr %s %s))"
                             var (smRA sm) argC
            pure smNewVar{smRC = var}
    _ -> error $ "Invalid opcode " ++ show instr
  where
    instr = code V.! smIP sm
    arg = code V.! (smIP sm + 1)
    argL = sLit arg
    argC = case arg of
        _ | arg >= 0 && arg <= 3 -> sLit arg
        4 -> smRA sm
        5 -> smRB sm
        6 -> smRC sm
        _ -> error $ "Invalid combo arg " ++ show arg
    var = "x" ++ show (smNextVar sm)
    smNewVar = sm{smNextVar = smNextVar sm + 1}

haltSM :: Vector Int -> SM -> Bool
haltSM code SM{smIP = ip} = ip < 0 || ip >= V.length code

runSM :: Monad m => Vector Int -> SM -> Stream (Of String) m SM
runSM code sm
  | haltSM code sm = pure sm
  | otherwise = do
        sm' <- stepSM code sm
        runSM code sm'{smIP = smIP sm' + 2}

runSMQuine :: Monad m => Vector Int -> VM -> Int -> Stream (Of String) m ()
runSMQuine code vm maxA0 = do
    S.yield "(set-option :pp.bv-literals false)"
    let sm = SM{
        smIP = ip vm, smRA = "a0", smRB = "b0", smRC = "c0",
        smNextVar = 0, smExpectedOuts = V.toList code}
    S.yield "(declare-const a0 (_ BitVec 64))"
    S.yield $ printf "(assert (bvult a0 (_ bv%d 64)))" maxA0
    S.yield $ printf "(define-const b0 (_ BitVec 64) %s)" (sLit $ rb vm)
    S.yield $ printf "(define-const c0 (_ BitVec 64) %s)" (sLit $ rc vm)
    _ <- runSM code sm
    S.yield "(check-sat)"
    S.yield "(get-value (a0))"

smtResultP :: Parser (Maybe Int)
smtResultP = sat <|> unsat
  where
    sat = do
        _ <- P.string "sat\n"
        a0 <- P.string "((a0 (_ bv" *> P.decimal <* P.string " 64)))\n"
        P.eof
        pure (Just a0)
    unsat = Nothing <$ P.string "unsat"

runSMT :: Vector Int -> VM -> Int -> IO (Maybe Int)
runSMT code vm maxA0 = do
    (Just z3in, Just z3out, Nothing, _) <- createProcess
        (proc "z3" ["-smt2", "-in"]){std_in = CreatePipe, std_out = CreatePipe}
    S.toHandle z3in (runSMQuine code vm maxA0)
    hClose z3in
    output <- T.hGetContents z3out
    case P.parse smtResultP "" output of
        Left err -> fail (P.errorBundlePretty err)
        Right result -> pure result

findMinSMT :: Vector Int -> VM -> IO Int
findMinSMT code vm = go maxBound
  where
    go maxA0 = do
        printf "Running SMT solver with maxA0 = %d\n" maxA0
        result <- runSMT code vm maxA0
        case result of
            Nothing -> pure maxA0
            Just a0 -> go a0

main :: IO ()
main = do
    (vm, code) <- parseInput 17 vmP
    print $ getOutput code vm
    a0 <- findMinSMT code vm
    print a0