{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module B.Monad
  ( Build
  , BuildRule
  , runBuild
  , withRule
  , getRuleDatabase
  , getOracle
  , getQuestion
  , liftBuild
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import B.Oracle
import B.Question
import B.RuleDatabase

newtype Build m a = Build (ReaderT (BuildEnv m) m a)
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  )

instance MonadTrans Build where
  lift = Build . lift

data BuildEnv m = BuildEnv
  { ruleDatabase :: RuleDatabase m
  , oracle :: Oracle m
  }

newtype BuildRule m a = BuildRule (ReaderT AQuestion (Build m) a)
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  )

runBuild :: RuleDatabase m -> Oracle m -> Build m a -> m a
runBuild ruleDatabase' oracle' (Build m)
  = runReaderT m BuildEnv
    { ruleDatabase = ruleDatabase'
    , oracle = oracle'
    }

withRule :: (Question q) => q -> BuildRule m a -> Build m a
withRule q (BuildRule m) = runReaderT m $ AQuestion q

getRuleDatabase :: (Monad m) => Build m (RuleDatabase m)
getRuleDatabase = Build $ asks ruleDatabase

getOracle :: (Monad m) => Build m (Oracle m)
getOracle = Build $ asks oracle

getQuestion :: (Monad m) => BuildRule m AQuestion
getQuestion = BuildRule ask

liftBuild :: (Monad m) => Build m a -> BuildRule m a
liftBuild m = BuildRule $ lift m
