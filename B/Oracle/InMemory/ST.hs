module B.Oracle.InMemory.ST
  ( mkSTOracle
  ) where

import Control.Applicative
import Control.Monad.ST.Safe
import Data.STRef

import B.Oracle (Oracle)

import qualified B.Oracle as Oracle
import qualified B.Oracle.InMemory.Pure as Pure

mkSTOracle :: ST s (Oracle (ST s))
mkSTOracle = mkSTOracleWithStorage
  <$> newSTRef Pure.empty

mkSTOracleWithStorage
  :: STRef s (Pure.State (ST s))
  -> Oracle (ST s)
mkSTOracleWithStorage storage = Oracle.Oracle
  { Oracle.get = \ q
    -> Pure.get q <$> readSTRef storage
  , Oracle.put = \ q a
    -> modifySTRef storage (Pure.put q a)
  , Oracle.dirty = \ q
    -> modifySTRef storage (Pure.dirty q)
  , Oracle.addDependency = \ from to
    -> modifySTRef storage (Pure.addDependency from to)
  }
