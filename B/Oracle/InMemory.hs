{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module B.Oracle.InMemory
  ( mkOracle
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Data.Typeable

import B.Oracle (Oracle)
import B.Question
import Data.DynSet (DynSet)

import qualified B.Oracle as Oracle
import qualified Data.DynSet as DynSet

data QuestionAnswer q = QuestionAnswer
  { _qaQuestion :: q
  , qaAnswer :: Answer q
  } deriving (Typeable)

type Storage = DynSet Typeable

mkOracle :: IO (Oracle IO)
mkOracle = mkOracleWithStorage <$> newTVarIO DynSet.empty

mkOracleWithStorage :: TVar Storage -> Oracle IO
mkOracleWithStorage storageVar = Oracle.Oracle
  { Oracle.get = get
  , Oracle.put = put
  }

  where
    get
      :: forall q. (Question q)
      => q -> IO (Maybe (Answer q))
    get q = atomically $ do
      storage <- readTVar storageVar
      return $ qaAnswer
        <$> (DynSet.lookup storage :: Maybe (QuestionAnswer q))

    put
      :: forall q. (Question q)
      => q -> Answer q -> IO ()
    put q a = atomically
      $ modifyTVar storageVar
        (DynSet.insert (QuestionAnswer q a))

