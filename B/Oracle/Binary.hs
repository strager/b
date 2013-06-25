module B.Oracle.Binary
  ( Fingerprint(..)
  , putTypeOf
  , getTypeFingerprint
  ) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Typeable.Internal

putTypeOf :: (Typeable a) => a -> Put
putTypeOf x = case typeOf x of
  TypeRep fingerprint _ _ -> putFingerprint fingerprint

getTypeFingerprint :: Get Fingerprint
getTypeFingerprint = getFingerprint

putFingerprint :: Fingerprint -> Put
putFingerprint (Fingerprint a b)
  = putWord64le a *> putWord64le b

getFingerprint :: Get Fingerprint
getFingerprint = Fingerprint
  <$> getWord64le <*> getWord64le
