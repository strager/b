{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module B.Build
  ( NoRule(..)
  , build
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

data NoRule where
  NoRule :: (Question q) => q -> NoRule
  deriving (Typeable)

instance Eq NoRule where
  NoRule a == NoRule b = cast a == Just b

instance Show NoRule where
  showsPrec _ (NoRule q)
    = showString "No rule to build " . shows q

instance Exception NoRule

actuallyBuild
  :: (Question q, m ~ AnswerMonad q)
  => q -> Build m (Either SomeException (Answer q))
actuallyBuild q = do
  rules <- getRuleDatabase
  case executeRule q rules of
    Just m -> do
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
    Nothing -> return $ Left (SomeException (NoRule q))
