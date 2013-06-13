{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module B.Oracle.InMemory.STM
  ( mkSTMOracle
  , mkSTMStorage
  , mkSTMStorageIO
  , mkSTMOracleWithStorageIO
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import B.Oracle (Oracle)

import qualified B.Oracle.InMemory.Pure as Pure

mkSTMOracle :: (MonadIO m) => m (Oracle m)
mkSTMOracle = liftM mkSTMOracleWithStorageIO
  $ liftIO mkSTMStorageIO

mkSTMStorage :: STM (TVar (Pure.State m))
mkSTMStorage = newTVar Pure.empty

mkSTMStorageIO :: IO (TVar (Pure.State m))
mkSTMStorageIO = newTVarIO Pure.empty

mkSTMOracleWithStorageIO
  :: (MonadIO m)
  => TVar (Pure.State m)
  -> Oracle m
mkSTMOracleWithStorageIO storage = Pure.mkOracle
  (liftIO $ readTVarIO storage)
  (liftIO . atomically . modifyTVar storage)
