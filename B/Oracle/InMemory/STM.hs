{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module B.Oracle.InMemory.STM
  ( mkSTMOracle
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import B.Oracle (Oracle)

import qualified B.Oracle as Oracle
import qualified B.Oracle.InMemory.Pure as Pure

mkSTMOracle :: (MonadIO m) => m (Oracle m)
mkSTMOracle = liftM mkSTMOracleWithStorage
  $ liftIO (newTVarIO Pure.empty)

mkSTMOracleWithStorage
  :: (MonadIO m)
  => TVar (Pure.State m)
  -> Oracle m
mkSTMOracleWithStorage storage = Oracle.Oracle
  { Oracle.get = \ q
    -> liftM (Pure.get q)
    . liftIO $ readTVarIO storage
  , Oracle.put = \ q a -> liftIO . atomically
    $ modifyTVar storage (Pure.put q a)
  , Oracle.dirty = \ q -> liftIO . atomically
    $ modifyTVar storage (Pure.dirty q)
  , Oracle.addDependency = \ from to -> liftIO . atomically
    $ modifyTVar storage (Pure.addDependency from to)
  }
{-# ANN mkSTMOracleWithStorage ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket" :: String) #-}
