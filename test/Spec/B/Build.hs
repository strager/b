{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.B.Build
  ( spec
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Map (Map)
import Data.Semigroup
import Data.Typeable
import Test.Hspec

import qualified Data.Map as Map

import B.Build
import B.Log
import B.Monad
import B.Oracle (Oracle)
import B.Question
import B.Rule
import B.RuleDatabase (RuleDatabase)

import qualified B.Oracle.InMemory.Pure as PureOracle
import qualified B.RuleDatabase as RuleDatabase

newtype A = A String deriving (Eq, Ord, Show, Typeable)
newtype B = B String deriving (Eq, Ord, Show, Typeable)

instance Question A where
  type Answer A = String
  type AnswerMonad A = M
  answer (A x) = return x

instance Question B where
  type Answer B = String
  type AnswerMonad B = M
  answer (B x) = return $ reverse x

newtype M a = M
  { unM :: StateT (PureOracle.State M)
    (WriterT [LogMessage] IO) a
  } deriving (Functor, Applicative, Monad, MonadIO)

oracle :: Oracle M
oracle = PureOracle.mkOracle (M get) (M . modify)

-- | Questions and their dependencies.
data R q = R (Map q [AQuestion M])
  deriving (Typeable)

r :: (Ord q) => q -> [AQuestion M] -> R q
r q deps = R $ Map.singleton q deps

instance (Ord q) => Semigroup (R q) where
  R a <> R b = R $ Map.unionWith mappend a b

instance (Ord q, Question q, M ~ AnswerMonad q)
  => Rule q (R q) where
  executeRule q (R xs) = case Map.lookup q xs of
    Just deps -> Just
      $ mapM_ needAQuestion_ deps
    Nothing -> Nothing

needAQuestion_ :: AQuestion m -> BuildRule m ()
needAQuestion_ (AQuestion q) = need_ q

testBuild
  :: RuleDatabase M
  -> [LogMessage]
  -> Build M a
  -> Expectation
testBuild db expectedLogs m = do
  logMessages <- execWriterT
    $ evalStateT go PureOracle.empty 
  logMessages `shouldBe` expectedLogs
  where
  logger message = M . lift $ tell [message]
  go = void . unM $ runBuild db oracle logger m

spec :: Spec
spec = do
  it "No rule" $ testBuild
    RuleDatabase.empty
    [NoRuleError (A "hi")]
    $ build (A "hi")

  it "No rule for dependency" $ testBuild
    (RuleDatabase.singleton $ r (A "hi") [AQuestion $ A "bye"])
    [Building (A "hi"), NoRuleError (A "bye"), DoneBuilding (A "hi")]
    $ build (A "hi")

  it "Build one" $ testBuild
    (RuleDatabase.singleton $ r (A "hi") [])
    [Building (A "hi"), DoneBuilding (A "hi")]
    $ build (A "hi")

  it "Build first rule" $ testBuild
    (RuleDatabase.fromList [r (A "1") [], r (A "2") [], r (A "3") []])
    [Building (A "1"), DoneBuilding (A "1")]
    $ build (A "1")

  it "Build second rule" $ testBuild
    (RuleDatabase.fromList [r (A "1") [], r (A "2") [], r (A "3") []])
    [Building (A "2"), DoneBuilding (A "2")]
    $ build (A "2")

  it "Build third rule" $ testBuild
    (RuleDatabase.fromList [r (A "1") [], r (A "2") [], r (A "3") []])
    [Building (A "3"), DoneBuilding (A "3")]
    $ build (A "3")
