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
  { unM :: WriterT [LogMessage]
    (StateT (PureOracle.State M) IO) a
  } deriving (Functor, Applicative, Monad, MonadIO)

-- | Questions and their dependencies.
data R q = R (Map q [AQuestion M])
  deriving (Typeable)

rdb
  :: (Ord q, Question q, M ~ AnswerMonad q)
  => q -> [AQuestion M] -> RuleDatabase M
rdb q deps = RuleDatabase.singleton
  . R $ Map.singleton q deps

instance (Ord q) => Semigroup (R q) where
  R x <> R y = R $ Map.unionWith mappend x y

instance (Ord q, Question q, M ~ AnswerMonad q)
  => Rule q (R q) where
  executeRule q (R xs) = case Map.lookup q xs of
    Just deps -> Just
      $ mapM_ needAQuestion_ deps
    Nothing -> Nothing

needAQuestion_ :: AQuestion m -> BuildRule m ()
needAQuestion_ (AQuestion q) = need_ q

testBuild
  :: [RuleDatabase M]
  -> [[LogMessage]]
  -> Build M a
  -> Expectation
testBuild dbs expectedLogs m = evalStateT
  (mapM_ testOneBuild expectedLogs) PureOracle.empty
  where
  testOneBuild expected = do
    logMessages <- execWriterT go
    lift $ logMessages `shouldBe` expected

  go = void . unM $ runBuild db oracle logger m
  db = mconcat dbs
  oracle = PureOracle.mkOracle
    (M $ lift get) (M . lift . modify)
  logger message = M $ tell [message]

a :: String -> AQuestion M
a = AQuestion . A

b :: String -> AQuestion M
b = AQuestion . B

spec :: Spec
spec = do
  it "No rule" $ testBuild
    []
    [ [NoRuleError (A "hi")]
    , [NoRuleError (A "hi")]
    ]
    $ build (A "hi")

  it "No rule for dependency" $ testBuild
    [rdb (A "hi") [AQuestion $ A "bye"]]
    [ [ Building (A "hi")
      , NoRuleError (A "bye")
      , DoneBuilding (A "hi")
      ]
    , [AlreadyBuilt (A "hi")]
    ] $ build (A "hi")

  it "Build one" $ testBuild
    [rdb (A "hi") []]
    [ [Building (A "hi"), DoneBuilding (A "hi")]
    , [AlreadyBuilt (A "hi")]
    ]
    $ build (A "hi")

  it "Build first rule" $ testBuild
    [ rdb (A "1") []
    , rdb (A "2") []
    , rdb (A "3") []
    ]
    [ [Building (A "1"), DoneBuilding (A "1")]
    , [AlreadyBuilt (A "1")]
    ]
    $ build (A "1")

  it "Build second rule" $ testBuild
    [ rdb (A "1") []
    , rdb (A "2") []
    , rdb (A "3") []
    ]
    [ [Building (A "2"), DoneBuilding (A "2")]
    , [AlreadyBuilt (A "2")]
    ]
    $ build (A "2")

  it "Build third rule" $ testBuild
    [ rdb (A "1") []
    , rdb (A "2") []
    , rdb (A "3") []
    ]
    [ [Building (A "3"), DoneBuilding (A "3")]
    , [AlreadyBuilt (A "3")]
    ]
    $ build (A "3")

  it "Chain with same rule type" $ testBuild
    [ rdb (A "1") [a "2"]
    , rdb (A "2") [a "3"]
    , rdb (A "3") [a "4"]
    , rdb (A "4") []
    ]
    [ [ Building (A "1")
      , Building (A "2")
      , Building (A "3")
      , Building (A "4")
      , DoneBuilding (A "4")
      , DoneBuilding (A "3")
      , DoneBuilding (A "2")
      , DoneBuilding (A "1")
      ]
    , [AlreadyBuilt (A "1")]
    ] $ build (A "1")

  it "Chain with interleaved rule type" $ testBuild
    [ rdb (A "1") [b "2"]
    , rdb (B "2") [a "3"]
    , rdb (A "3") [b "4"]
    , rdb (B "4") []
    ]
    [ [ Building (A "1")
      , Building (B "2")
      , Building (A "3")
      , Building (B "4")
      , DoneBuilding (B "4")
      , DoneBuilding (A "3")
      , DoneBuilding (B "2")
      , DoneBuilding (A "1")
      ]
    , [AlreadyBuilt (A "1")]
    ] $ build (A "1")
