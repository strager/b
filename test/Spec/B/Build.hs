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
import Control.Monad.Trans.Writer
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Semigroup
import Data.Typeable
import Test.Hspec
import Control.Monad.Trans.State (StateT, evalStateT)

import qualified Control.Monad.Trans.State as State
import qualified Data.Binary as Binary
import qualified Data.Map as Map

import B.Build
import B.Log
import B.Question
import B.Rule
import B.RuleDatabase (RuleDatabase)

import qualified B.Monad as B
import qualified B.Oracle.InMemory.Pure as PureOracle
import qualified B.RuleDatabase as RuleDatabase

newtype A = A String deriving (Eq, Ord, Show, Typeable)
newtype B = B String deriving (Eq, Ord, Show, Typeable)

instance Binary A where
  get = fmap A Binary.get
  put (A x) = Binary.put x

instance Question A where
  type Answer A = String
  type AnswerMonad A = M
  answer (A x) = return $ Right x

instance Binary B where
  get = fmap B Binary.get
  put (B x) = Binary.put x

instance Question B where
  type Answer B = String
  type AnswerMonad B = M
  answer (B x) = return . Right $ reverse x

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
  queryRule q (R xs) = case Map.lookup q xs of
    Just deps -> [mapM_ needAQuestion_ deps]
    Nothing -> []

needAQuestion_ :: AQuestion m -> B.BuildRule m ()
needAQuestion_ (AQuestion q) = need_ q

testBuild
  :: [RuleDatabase M]
  -> [[LogMessage]]
  -> B.Build M a
  -> Expectation
testBuild dbs expectedLogs m = evalStateT
  (mapM_ testOneBuild expectedLogs) PureOracle.empty
  where
  testOneBuild expected = do
    logMessages <- execWriterT go
    lift $ logMessages `shouldBe` expected

  go = void . unM $ B.runBuild B.BuildEnv
    { B.ruleDatabase = db
    , B.oracle = oracle
    , B.logger = logger
    , B.latch = B.defaultLatch
    , B.parallel = B.defaultParallel
    } m
  db = mconcat dbs
  oracle = PureOracle.mkOracle ask modify
  logger message = M $ tell [message]

  ask = liftState State.get
  modify f = liftState . State.put =<< f =<< liftState State.get
  liftState = M . lift

a :: String -> AQuestion M
a = AQuestion . A

b :: String -> AQuestion M
b = AQuestion . B

spec :: Spec
spec = do
  it "No rule" $ testBuild
    []
    [ [Exception . show $ NoRule (A "hi")]
    , [Exception . show $ NoRule (A "hi")]
    ] $ build (A "hi")

  it "No rule for dependency" $ testBuild
    [rdb (A "hi") [AQuestion $ A "bye"]]
    [ [ Building (A "hi")
      , BuildingDependencies (A "hi") [a "bye"]
      , Exception . show $ NoRule (A "bye")
      ]
    , [ Building (A "hi")
      , BuildingDependencies (A "hi") [a "bye"]
      , Exception . show $ NoRule (A "bye")
      ]
    ] $ build (A "hi")

  it "Build one" $ testBuild
    [rdb (A "hi") []]
    [ [Building (A "hi"), DoneBuilding (A "hi")]
    , [AlreadyBuilt (A "hi")]
    ] $ build (A "hi")

  it "Build first rule" $ testBuild
    [ rdb (A "1") []
    , rdb (A "2") []
    , rdb (A "3") []
    ]
    [ [Building (A "1"), DoneBuilding (A "1")]
    , [AlreadyBuilt (A "1")]
    ] $ build (A "1")

  it "Build second rule" $ testBuild
    [ rdb (A "1") []
    , rdb (A "2") []
    , rdb (A "3") []
    ]
    [ [Building (A "2"), DoneBuilding (A "2")]
    , [AlreadyBuilt (A "2")]
    ] $ build (A "2")

  it "Build third rule" $ testBuild
    [ rdb (A "1") []
    , rdb (A "2") []
    , rdb (A "3") []
    ]
    [ [Building (A "3"), DoneBuilding (A "3")]
    , [AlreadyBuilt (A "3")]
    ] $ build (A "3")

  it "Chain with same rule type" $ testBuild
    [ rdb (A "1") [a "2"]
    , rdb (A "2") [a "3"]
    , rdb (A "3") [a "4"]
    , rdb (A "4") []
    ]
    [ [ Building (A "1")
      , BuildingDependencies (A "1") [a "2"]
      , Building (A "2")
      , BuildingDependencies (A "2") [a "3"]
      , Building (A "3")
      , BuildingDependencies (A "3") [a "4"]
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
      , BuildingDependencies (A "1") [b "2"]
      , Building (B "2")
      , BuildingDependencies (B "2") [a "3"]
      , Building (A "3")
      , BuildingDependencies (A "3") [b "4"]
      , Building (B "4")
      , DoneBuilding (B "4")
      , DoneBuilding (A "3")
      , DoneBuilding (B "2")
      , DoneBuilding (A "1")
      ]
    , [AlreadyBuilt (A "1")]
    ] $ build (A "1")
