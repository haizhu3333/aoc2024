module Textlib (
    parseInt
) where

import Data.Text (Text)
import qualified Data.Text.Read as T

parseInt :: Integral a => Text -> a
parseInt t = case T.decimal t of
    Left err -> error $ "Cannot parse int: " ++ show err
    Right (x, "") -> x
    Right (_, leftover) -> error $ "Incomplete parse: " ++ show leftover