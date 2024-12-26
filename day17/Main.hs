module Main (main) where

import Control.Monad (void)
import Data.Bits (Bits(..))
import qualified Data.Vector.Unboxed as U
import Effectful (Eff, (:>), runPureEff)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.Printf (printf)

import OutputEff (Output, output, toList)
import SMT (runSMT)
import Utils (Parser, parseInput)

data VM = VM { ip :: !Int, ra :: !Int, rb :: !Int, rc :: !Int }
    deriving Show

regP :: Char -> Parser Int
regP ch = P.string "Register " *> P.char ch *> P.string ": "
       *> P.decimal <* P.newline

vmP :: Parser (VM, U.Vector Int)
vmP = do
    ra <- regP 'A'
    rb <- regP 'B'
    rc <- regP 'C'
    _ <- P.string "\nProgram: "
    code <- P.decimal `P.sepBy` P.char ',' <* P.newline
    pure (VM{ip = 0, ..}, U.fromList code)

halt :: U.Vector Int -> VM -> Bool
halt code VM{ip} = ip < 0 || ip >= U.length code

comboArg :: VM -> Int -> Int
comboArg VM{..} arg = case arg of
    _ | arg >= 0 && arg <= 3 -> arg
    4 -> ra
    5 -> rb
    6 -> rc
    _ -> error $ "Invalid combo arg " ++ show arg

step :: Output Int :> es => U.Vector Int -> VM -> Eff es VM
step code vm = case instr of
    0 -> pure vm{ra = ra vm `shiftR` argC}
    1 -> pure vm{rb = rb vm `xor` argL}
    2 -> pure vm{rb = argC .&. 7}
    3 | ra vm == 0 -> pure vm
      | otherwise  -> pure vm{ip = argL - 2}
    4 -> pure vm{rb = rb vm `xor` rc vm}
    5 -> output (argC .&. 7) *> pure vm
    6 -> pure vm{rb = ra vm `shiftR` argC}
    7 -> pure vm{rc = ra vm `shiftR` argC}
    _ -> error $ "Invalid opcode " ++ show instr
  where
    instr = code U.! ip vm
    argL = code U.! (ip vm + 1)
    argC = comboArg vm argL

run :: Output Int :> es => U.Vector Int -> VM -> Eff es VM
run code vm
  | halt code vm = pure vm
  | otherwise = do
        vm' <- step code vm
        run code vm'{ip = ip vm' + 2}

getOutput :: U.Vector Int -> VM -> [Int]
getOutput code vm = runPureEff $ toList (void $ run code vm)

findMinSMT :: U.Vector Int -> VM -> IO Int
findMinSMT code VM{..} = go maxBound
  where
    go maxA0 = do
        printf "Running SMT solver with maxA0 = %d\n" maxA0
        result <- runSMT code maxA0 rb rc
        case result of
            Nothing -> pure maxA0
            Just a0 -> go a0

main :: IO ()
main = do
    (vm, code) <- parseInput 17 vmP
    print $ getOutput code vm
    a0 <- findMinSMT code vm
    print a0