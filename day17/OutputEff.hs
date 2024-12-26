{-# LANGUAGE LambdaCase, DataKinds, TypeFamilies #-}
module OutputEff (Output, output, toHandle, toList) where

import Data.Monoid (Endo(..))
import Effectful (Effect, Eff, (:>), DispatchOf, Dispatch(..), IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret, reinterpret, send)
import Effectful.Writer.Static.Local (runWriter, tell)
import System.IO (Handle, hPutStrLn)

data Output o :: Effect where
    Output :: o -> Output o m ()

type instance DispatchOf (Output o) = Dynamic

output :: Output o :> es => o -> Eff es ()
output = send . Output

toHandle :: IOE :> es => Handle -> Eff (Output String : es) a -> Eff es a
toHandle h = interpret $ \_ (Output x) -> liftIO $ hPutStrLn h x

toList :: Eff (Output o : es) () -> Eff es [o]
toList = reinterpret get $ \_ (Output x) -> tell (Endo (x :))
  where
    get = fmap (flip appEndo [] . snd) . runWriter
