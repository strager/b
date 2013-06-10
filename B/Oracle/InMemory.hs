{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module B.Oracle.InMemory
  ( mkOracle
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Data.Typeable

import qualified Data.Maybe as Maybe

import B.Oracle (Oracle)
import B.Question
import B.RuleSet

import qualified B.Oracle as Oracle

data QuestionAnswer where
  QuestionAnswer
    :: (Question q)
    => q -> Answer q -> QuestionAnswer
  deriving (Typeable)

data Dependency where
  Dependency
    :: (RuleSet from r, Question to)
    => from -> to -> r -> Dependency
  deriving (Typeable)

mkOracle :: IO (Oracle IO)
mkOracle = mkOracleWithStorage
  <$> newTVarIO []
  <*> newTVarIO []

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f = Maybe.listToMaybe . Maybe.mapMaybe f

mkOracleWithStorage
  :: TVar [QuestionAnswer]
  -> TVar [Dependency]
  -> Oracle IO
mkOracleWithStorage qaStorage depStorage = Oracle.Oracle
  { Oracle.get = get
  , Oracle.put = put
  , Oracle.addDependency = addDependency
  }

  where
    get
      :: forall q. (Question q)
      => q -> IO (Maybe (Answer q))
    get q = atomically
      $ findJust f <$> readTVar qaStorage
      where
      f (QuestionAnswer q' a)
        | cast q == Just q' = cast a
      f _ = Nothing

    put
      :: forall q. (Question q)
      => q -> Answer q -> IO ()
    put q a = atomically
      $ modifyTVar qaStorage (QuestionAnswer q a :)

    addDependency
      :: forall from to r. (RuleSet from r, Question to)
      => from -> to -> r -> IO ()
    addDependency from to ruleSet = atomically
      $ modifyTVar depStorage (Dependency from to ruleSet :)
