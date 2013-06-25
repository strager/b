module B.Util.Binary
  ( Sized(..)

  , getCounted
  , putCounted
  ) where

import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.List (genericLength)

newtype Sized a = Sized a

instance (Binary a) => Binary (Sized a) where
  get = fmap (Sized . decode) get
  put (Sized x) = put $ encode x

getCounted :: Get a -> Get [a]
getCounted getOne = do
  count <- fmap fromIntegral getWord32le
  replicateM count getOne

putCounted :: (a -> Put) -> [a] -> Put
putCounted f xs = do
  putWord32le $ genericLength xs
  mapM_ f xs
