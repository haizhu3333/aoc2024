module Main (main) where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Utils (Parser, parseInput)

mulInstr :: Parser (Int, Int)
mulInstr =  (,) <$> (P.string "mul(" *> P.decimal <* P.string ",")
                <*> (P.decimal <* P.string ")")

data Instr = Mul !Int !Int | SetEnabled !Bool

fullInstr :: Parser Instr
fullInstr = P.choice
    [ uncurry Mul <$> mulInstr
    , SetEnabled True <$ P.string "do()"
    , SetEnabled False <$ P.string "don't()"
    ]

multi :: Parser a -> Parser [a]
multi instr = go
  where
    go = P.choice
        [ (:) <$> P.try instr <*> go
        , P.anySingle *> go
        , [] <$ P.eof
        ]

evalFull :: [Instr] -> Int
evalFull = go 0 True
  where
    go !acc _ [] = acc
    go !acc enabled (Mul x y : instrs) =
        let acc' = acc + (if enabled then x * y else 0)
        in  go acc' enabled instrs
    go !acc _ (SetEnabled enabled : instrs) = go acc enabled instrs

main :: IO ()
main = do
    instrs1 <- parseInput 3 (multi mulInstr)
    print $ sum $ map (uncurry (*)) instrs1
    instrs2 <- parseInput 3 (multi fullInstr)
    print $ evalFull instrs2
