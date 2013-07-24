{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module B.Build
  ( NoRule(..)
  , build
  , build_
  , need
  , needs
  , need_
  , needs_
  ) where

import Control.Exception (Exception, SomeException(..))
import Control.Monad
import Control.Monad.Trans.Class
import Data.Typeable

import B.Log
import B.Monad (Build, BuildRule)
import B.Question
import B.Rule

import qualified B.Monad as B
import qualified B.Oracle as Oracle
import qualified B.Parallel as Par

need
  :: (Monad m, Question q, m ~ AnswerMonad q)
  => q -> BuildRule m (Answer q)
need q = do
  AQuestion from <- B.getQuestion
  B.liftBuild . B.logBuild
    $ BuildingDependencies from [AQuestion q]
  need1 q

-- | Like 'need', but does not perform logging.  Private.
need1
  :: (Monad m, Question q, m ~ AnswerMonad q)
  => q -> BuildRule m (Answer q)
need1 q = do
  oracle <- B.liftBuild B.getOracle
  AQuestion from <- B.getQuestion
  lift $ Oracle.addDependency oracle from q
  B.liftBuild $ build q

-- | Like 'mapM' 'need', but allowing for parallelism.
needs
  :: (Monad m, Question q, m ~ AnswerMonad q)
  => [q] -> BuildRule m [Answer q]
needs qs = do
  AQuestion from <- B.getQuestion
  B.liftBuild . B.logBuild
    $ BuildingDependencies from (map AQuestion qs)

  par <- B.getBuildRuleParallel
  Par.mapM par need1 qs

-- | Like 'need', but ignoring the resulting answer.
need_
  :: (Monad m, Question q, m ~ AnswerMonad q)
  => q -> BuildRule m ()
need_ = liftM (const ()) . need

-- | Like 'mapM_' 'need_', but allowing for parallelism.
needs_
  :: (Monad m)
  => [AQuestion m] -> BuildRule m ()
needs_ qs = do
  par <- B.getBuildRuleParallel
  Par.mapM_ par f qs
  where
  f :: (Monad m) => AQuestion m -> BuildRule m ()
  f (AQuestion q) = need_ q

build
  :: (Monad m, Question q, m ~ AnswerMonad q)
  => q -> Build m (Answer q)
build q = flip B.latchBuild q $ do
  oracle <- B.getOracle
  mExistingAnswer <- lift $ Oracle.get oracle q
  case mExistingAnswer of
    Just existingAnswer -> do
      B.logBuild $ AlreadyBuilt q
      return existingAnswer
    Nothing -> actuallyBuild q
      >>= either handleError return

  where
  handleError :: (Monad m) => SomeException -> Build m a
  handleError ex = do
    B.logBuild $ Exception (show ex)
    B.throwBuild ex

build_
  :: (Monad m, Question q, m ~ AnswerMonad q)
  => q -> Build m ()
build_ = liftM (const ()) . build

data NoRule where
  NoRule :: (Question q) => q -> NoRule
  deriving (Typeable)

instance Eq NoRule where
  NoRule a == NoRule b = cast a == Just b

instance Show NoRule where
  showsPrec _ (NoRule q)
    = showString "No rule to build " . shows q

instance Exception NoRule

data TooManyRules where
  TooManyRules :: (Question q) => q -> TooManyRules
  deriving (Typeable)

instance Eq TooManyRules where
  TooManyRules a == TooManyRules b = cast a == Just b

instance Show TooManyRules where
  showsPrec _ (TooManyRules q)
    = showString "Multiple rules to build " . shows q

instance Exception TooManyRules

actuallyBuild
  :: (Question q, m ~ AnswerMonad q)
  => q -> Build m (Either SomeException (Answer q))
actuallyBuild q = do
  database <- B.getRuleDatabase
  case queryRule q database of
    [m] -> do
      B.logBuild $ Building q
      B.withRule q m
      mAns <- lift $ answer q
      case mAns of
        Left ex -> return $ Left ex
        Right ans -> do
          oracle <- B.getOracle
          lift $ Oracle.put oracle q ans
          B.logBuild $ DoneBuilding q
          return $ Right ans
    [] -> return $ Left (SomeException (NoRule q))
    _rules -> return $ Left (SomeException (TooManyRules q))
