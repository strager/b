module B.Oracle.InMemory.MVar
  ( mkMVarOracle
  , mkMVarStorage
  , mkMVarOracleWithStorage
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.IO.Class

import B.Oracle (Oracle)

import qualified B.Oracle.InMemory.Pure as Pure

mkMVarOracle :: (MonadIO m) => IO (Oracle m)
mkMVarOracle = mkMVarOracleWithStorage <$> mkMVarStorage

mkMVarStorage :: (MonadIO m) => IO (MVar (Pure.State m))
mkMVarStorage = newMVar Pure.empty

mkMVarOracleWithStorage
  :: (MonadIO m)
  => MVar (Pure.State m)
  -> Oracle m
mkMVarOracleWithStorage storage = Pure.mkOracle ask modify
  where
  ask = liftIO $ readMVar storage
  modify f = do
    x <- liftIO $ takeMVar storage
    x' <- f x
    liftIO $ putMVar storage x'
