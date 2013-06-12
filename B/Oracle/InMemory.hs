{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module B.Oracle.InMemory
  ( mkOracle
  ) where

import Control.Applicative
import Control.Concurrent.STM

import B.Oracle (Oracle)

import qualified B.Oracle as Oracle
import qualified B.Oracle.InMemory.Pure as Pure

mkOracle :: IO (Oracle IO)
mkOracle = mkOracleWithStorage
  <$> newTVarIO Pure.empty

mkOracleWithStorage
  :: TVar (Pure.State IO)
  -> Oracle IO
mkOracleWithStorage storage = Oracle.Oracle
  { Oracle.get = \ q
    -> Pure.get q <$> readTVarIO storage
  , Oracle.put = \ q a -> atomically
    $ modifyTVar storage (Pure.put q a)
  , Oracle.dirty = \ q -> atomically
    $ modifyTVar storage (Pure.dirty q)
  , Oracle.addDependency = \ from to -> atomically
    $ modifyTVar storage (Pure.addDependency from to)
  }
