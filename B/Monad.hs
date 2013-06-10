{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module B.Monad
  ( Build
  , BuildRule
  , BuildRuleEnv(..)
  , runBuild
  , withRule
  , getRuleDatabase
  , getOracle
  , getRule
  , liftBuild
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Typeable (Typeable)

import B.Oracle
import B.RuleDatabase
import B.RuleSet (RuleSet)

newtype Build a = Build (ReaderT BuildEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

data BuildEnv = BuildEnv
  { ruleDatabase :: RuleDatabase
  , oracle :: Oracle IO
  }

newtype BuildRule a = BuildRule (ReaderT BuildRuleEnv Build a)
  deriving (Functor, Applicative, Monad, MonadIO, Typeable)

data BuildRuleEnv where
  BuildRuleEnv
    :: (RuleSet q r)
    => q -> r -> BuildRuleEnv

runBuild :: RuleDatabase -> Oracle IO -> Build a -> IO a
runBuild ruleDatabase' oracle' (Build m)
  = runReaderT m BuildEnv
    { ruleDatabase = ruleDatabase'
    , oracle = oracle'
    }

withRule :: (RuleSet q r) => q -> r -> BuildRule a -> Build a
withRule q r (BuildRule m) = runReaderT m
  $ BuildRuleEnv q r

getRuleDatabase :: Build RuleDatabase
getRuleDatabase = Build $ asks ruleDatabase

getOracle :: Build (Oracle IO)
getOracle = Build $ asks oracle

getRule :: BuildRule BuildRuleEnv
getRule = BuildRule ask

liftBuild :: Build a -> BuildRule a
liftBuild m = BuildRule $ lift m
