{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module B.Build
  ( NoRule(..)
  , build
  , build_
  , need
  , need_
  ) where

import Control.Exception (Exception, SomeException(..))
import Control.Monad
import Control.Monad.Trans.Class
import Data.Typeable

import B.Log
import B.Monad
import B.Question
import B.Rule

import qualified B.Oracle as Oracle

need
  :: (Monad m, Question q, m ~ AnswerMonad q)
  => q -> BuildRule m (Answer q)
need q = do
  oracle <- liftBuild getOracle
  AQuestion from <- getQuestion
  liftBuild . lift $ Oracle.addDependency oracle from q
  liftBuild $ build q

need_
  :: (Monad m, Question q, m ~ AnswerMonad q)
  => q -> BuildRule m ()
need_ = liftM (const ()) . need

build
  :: (Monad m, Question q, m ~ AnswerMonad q)
  => q -> Build m (Answer q)
build q = do
  oracle <- getOracle
  mExistingAnswer <- lift $ Oracle.get oracle q
  case mExistingAnswer of
    Just existingAnswer -> do
      logBuild $ AlreadyBuilt q
      return existingAnswer
    Nothing -> actuallyBuild q
      >>= either handleError return

  where
  handleError :: (Monad m) => SomeException -> Build m a
  handleError ex = do
    logBuild $ Exception (show ex)
    throwBuild ex

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
  database <- getRuleDatabase
  case queryRule q database of
    [m] -> do
      logBuild $ Building q
      withRule q m
      mAns <- lift $ answer q
      case mAns of
        Left ex -> return $ Left ex
        Right ans -> do
          oracle <- getOracle
          lift $ Oracle.put oracle q ans
          logBuild $ DoneBuilding q
          return $ Right ans
    [] -> return $ Left (SomeException (NoRule q))
    _rules -> return $ Left (SomeException (TooManyRules q))
