{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
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
import Data.Typeable (Typeable)

import B.Oracle
import B.Question
import B.RuleDatabase

newtype Build a = Build (ReaderT BuildEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

data BuildEnv = BuildEnv
  { ruleDatabase :: RuleDatabase
  , oracle :: Oracle IO
  }

newtype BuildRule a = BuildRule (ReaderT AQuestion Build a)
  deriving (Functor, Applicative, Monad, MonadIO, Typeable)

runBuild :: RuleDatabase -> Oracle IO -> Build a -> IO a
runBuild ruleDatabase' oracle' (Build m)
  = runReaderT m BuildEnv
    { ruleDatabase = ruleDatabase'
    , oracle = oracle'
    }

withRule :: (Question q) => q -> BuildRule a -> Build a
withRule q (BuildRule m) = runReaderT m
  $ AQuestion q

getRuleDatabase :: Build RuleDatabase
getRuleDatabase = Build $ asks ruleDatabase

getOracle :: Build (Oracle IO)
getOracle = Build $ asks oracle

getQuestion :: BuildRule AQuestion
getQuestion = BuildRule ask

liftBuild :: Build a -> BuildRule a
liftBuild m = BuildRule $ lift m
