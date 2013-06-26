module B.Oracle.InMemory.MVar
  ( mkMVarOracle
  , mkMVarStorage
  , mkMVarOracleWithStorage
  ) where

import Control.Applicative
import Control.Concurrent.MVar

import B.Oracle (Oracle)

import qualified B.Oracle.InMemory.Pure as Pure

mkMVarOracle :: IO (Oracle IO)
mkMVarOracle = mkMVarOracleWithStorage <$> mkMVarStorage

mkMVarStorage :: IO (MVar (Pure.State IO))
mkMVarStorage = newMVar Pure.empty

mkMVarOracleWithStorage
  :: MVar (Pure.State IO)
  -> Oracle IO
mkMVarOracleWithStorage storage = Pure.mkOracle ask modify
  where
  ask = readMVar storage
  modify = modifyMVar_ storage
