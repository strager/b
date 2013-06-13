{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Typeable
import Test.Hspec

import B.Build
import B.Log
import B.Monad
import B.Oracle (Oracle)
import B.Question
import B.Rule
import B.RuleDatabase (RuleDatabase)

import qualified B.Oracle.InMemory.Pure as PureOracle
import qualified B.RuleDatabase as RuleDatabase

newtype A = A String deriving (Eq, Show, Typeable)
newtype B = B String deriving (Eq, Show, Typeable)

instance Question M A where
  type Answer A = String
  answer (A x) = return x

instance Question M B where
  type Answer B = String
  answer (B x) = return $ reverse x

newtype M a = M
  { unM :: StateT (PureOracle.State M)
    (WriterT [LogMessage M] IO) a
  } deriving (Functor, Applicative, Monad, MonadIO)

oracle :: Oracle M
oracle = PureOracle.mkOracle (M get) (M . modify)

-- | Question and its dependencies.
data R q = R q [AQuestion M]
  deriving (Typeable)

instance (Question M q) => Rule q M (R q) where
  executeRule q (R q' deps)
    | cast q == Just q' = Just
      $ mapM_ needAQuestion_ deps
    | otherwise = Nothing

needAQuestion_ :: AQuestion m -> BuildRule m ()
needAQuestion_ (AQuestion q) = need_ q

testBuild
  :: RuleDatabase M
  -> [LogMessage M]
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
  it "foo" $ testBuild
    (RuleDatabase.singleton $ R (A "hi") [])
    [Building (A "hi"), DoneBuilding (A "hi")]
    $ build (A "hi")
