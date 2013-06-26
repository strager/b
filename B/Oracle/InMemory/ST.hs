module B.Oracle.InMemory.ST
  ( mkSTOracle
  , mkSTStorage
  , mkSTOracleWithStorage
  ) where

import Control.Applicative
import Control.Monad.ST.Safe
import Data.STRef

import B.Oracle (Oracle)

import qualified B.Oracle.InMemory.Pure as Pure

mkSTOracle :: ST s (Oracle (ST s))
mkSTOracle = mkSTOracleWithStorage <$> mkSTStorage

mkSTStorage :: ST s (STRef s (Pure.State (ST s)))
mkSTStorage = newSTRef Pure.empty

mkSTOracleWithStorage
  :: STRef s (Pure.State (ST s))
  -> Oracle (ST s)
mkSTOracleWithStorage storage = Pure.mkOracle ask modify
  where
  ask = readSTRef storage
  modify f = writeSTRef storage =<< f =<< readSTRef storage
