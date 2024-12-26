{-# LANGUAGE StrictData, LambdaCase, DataKinds, TypeFamilies #-}
module SMT (runSMT) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import qualified Data.Vector.Unboxed as U
import Data.List (intercalate)
import qualified Data.Text.IO as T
import Effectful (Effect, Eff, (:>), DispatchOf, Dispatch(..), runEff)
import Effectful.Dispatch.Dynamic (send, reinterpret)
import Effectful.Reader.Static (Reader, runReader, asks, local)
import Effectful.State.Static.Local (State, evalState, get, put)
import System.IO (Handle, hClose)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..))
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Text.Megaparsec as P
import Text.Printf (printf)

import OutputEff (Output, output, toHandle)
import Utils (Parser)

data Expr = Var String | Lit Int | LitB Bool  | App String [Expr]
    deriving Show

data SM = SM { ip :: Int, ra :: Expr, rb :: Expr, rc :: Expr }
    deriving Show

data Ctx = Ctx { branch :: Expr, outputs :: [Int] }
    deriving Show

data SMTGen :: Effect where
    DefConst :: Expr -> SMTGen m Expr
    DefBranch :: Expr -> SMTGen m Expr
    AssertE :: Expr -> SMTGen m ()

type instance DispatchOf SMTGen = Dynamic

defBranch :: SMTGen :> es => Expr -> Eff es Expr
defBranch = send . DefBranch

defConst :: SMTGen :> es => Expr -> Eff es Expr
defConst = send . DefConst

assertE :: SMTGen :> es => Expr -> Eff es ()
assertE = send . AssertE

getOpcodeAndArg :: U.Vector Int -> SM -> Maybe (Int, Int)
getOpcodeAndArg code SM{ip}
  | ip < 0 || ip >= U.length code - 1 = Nothing
  | otherwise = Just (code U.! ip, code U.! (ip + 1))

bad :: SMTGen :> es => Eff es ()
bad = assertE (LitB False)

runSM :: (Reader Ctx :> es, SMTGen :> es) => U.Vector Int -> SM -> Eff es ()
runSM code sm = case getOpcodeAndArg code sm of
    Nothing -> do
        os <- asks outputs
        unless (null os) bad
    Just (opcode, arg) -> stepSM code sm opcode arg

stepSM :: forall es. (Reader Ctx :> es, SMTGen :> es)
       => U.Vector Int -> SM -> Int -> Int -> Eff es ()
stepSM code sm opcode arg = case opcode of
    0 -> do
        x <- defConst $ App "bvlshr" [ra sm, combo]
        next sm{ra = x}
    1 -> do
        x <- defConst $ App "bvxor" [rb sm, Lit arg]
        next sm{rb = x}
    2 -> do
        x <- defConst $ App "bvand" [combo, Lit 7]
        next sm{rb = x}
    3 -> do
        b <- defBranch $ App "=" [ra sm, Lit 0]
        local (\ctx -> ctx{branch = b}) $ next sm
        local (\ctx -> ctx{branch = App "not" [b]}) $ runSM code sm{ip = arg}
    4 -> do
        x <- defConst $ App "bvxor" [rb sm, rc sm]
        next sm{rb = x}
    5 -> do
        outs <- asks outputs
        case outs of
            [] -> bad
            x : xs -> do
                assertE $ App "=" [App "bvand" [combo, Lit 7], Lit x]
                local (\ctx -> ctx{outputs = xs}) $ next sm
    6 -> do
        x <- defConst $ App "bvlshr" [ra sm, combo]
        next sm{rb = x}
    7 -> do
        x <- defConst $ App "bvlshr" [ra sm, combo]
        next sm{rc = x}
    _ -> error $ printf "Invalid opcode %d" opcode
  where
    combo = case arg of
        _ | arg >= 0 && arg <= 3 -> Lit arg
        4 -> ra sm
        5 -> rb sm
        6 -> rc sm
        _ -> error $ printf "Invalid combo arg %d" arg
    next :: SM -> Eff es ()
    next sm' = runSM code sm'{ip = ip sm' + 2}

render :: Expr -> String
render (Var v) = v
render (LitB True) = "true"
render (LitB False) = "false"
render (Lit x) = printf "(_ bv%d 64)" x
render (App op args) =
    printf "(%s %s)" op (intercalate " " $ map render args)

disp :: Output String :> es => String -> Eff es ()
disp = output @String

runSMTGen :: forall es a. (Reader Ctx :> es, Output String :> es)
          => Eff (SMTGen : es) a -> Eff es a
runSMTGen = reinterpret (evalState @Int 0) $ \_ -> \case
    DefBranch expr -> do
        var <- newVar "b"
        disp $ printf "(define-const %s Bool %s)" (render var) (render expr)
        pure var
    DefConst expr -> do
        var <- newVar "x"
        disp $ printf "(define-const %s (_ BitVec 64) %s)"
                      (render var) (render expr)
        pure var
    AssertE expr -> do
        b <- asks branch
        let expr' = case b of
                LitB True -> expr
                cond -> App "=>" [cond, expr]
        disp $ printf "(assert %s)" (render expr')
  where
    newVar :: String -> Eff (State Int : es) Expr
    newVar prefix = do
        i <- get @Int
        put $! i + 1
        pure $ Var (prefix ++ show i)

genSMT :: Handle -> U.Vector Int -> Int -> Int -> Int -> IO ()
genSMT h code maxA0 b0 c0 = runEff $ toHandle h $ do
    let a0 = Var "a0"
    disp "(set-option :pp.bv-literals false)"
    disp $ printf "(declare-const %s (_ BitVec 64))" (render a0)
    runReader ctx $ runSMTGen $ do
        assertE $ App "bvult" [a0, Lit maxA0]
        runSM code $ SM 0 a0 (Lit b0) (Lit c0)
    disp "(check-sat)"
    disp $ printf "(get-value (%s))" (render a0)
  where
    ctx = Ctx{branch = LitB True, outputs = U.toList code}

smtResultP :: Parser (Maybe Int)
smtResultP = sat <|> unsat
  where
    sat = do
        _ <- P.string "sat\n"
        a0 <- P.string "((a0 (_ bv" *> P.decimal <* P.string " 64)))\n"
        P.eof
        pure (Just a0)
    unsat = Nothing <$ P.string "unsat"

runSMT :: U.Vector Int -> Int -> Int -> Int -> IO (Maybe Int)
runSMT code maxA0 b0 c0 = do
    (Just z3in, Just z3out, Nothing, _) <- createProcess
        (proc "z3" ["-smt2", "-in"]){std_in = CreatePipe, std_out = CreatePipe}
    genSMT z3in code maxA0 b0 c0
    hClose z3in
    z3outText <- T.hGetContents z3out
    case P.parse smtResultP "" z3outText of
        Left err -> fail (P.errorBundlePretty err)
        Right result -> pure result
