module Vec (
    V2(..)
) where

import Data.Hashable (Hashable(..))

data V2 a = V2 !a !a
    deriving (Eq, Ord, Show, Functor)

instance Applicative V2 where
    pure x = V2 x x
    V2 f g <*> V2 x y = V2 (f x) (g y)

instance Num a => Num (V2 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (V2 a) where
    fromRational = pure . fromRational
    recip = fmap recip
    (/) = liftA2 (/)

instance Hashable a => Hashable (V2 a) where
    hashWithSalt s (V2 x y) = s `hashWithSalt` x `hashWithSalt` y