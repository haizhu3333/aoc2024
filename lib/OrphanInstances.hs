{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances () where

import Data.Hashable (Hashable(..))
import Data.Massiv.Array (Ix2(..))

instance Hashable Ix2 where
    hashWithSalt s (i :. j) = s `hashWithSalt` i `hashWithSalt` j