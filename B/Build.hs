module B.Build
  ( build
  , need
  , need_
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import qualified Control.Exception as Ex

import B.Monad
import B.Question
import B.Rule

import qualified B.Oracle as Oracle

need
  :: (MonadIO m, Monad m, Question q)
  => q -> BuildRule m (Answer q)
need q = do
  oracle <- liftBuild getOracle
  AQuestion from <- getQuestion
  liftBuild . lift $ Oracle.addDependency oracle from q
  liftBuild $ build q

need_
  :: (MonadIO m, Functor m, Monad m, Question q)
  => q -> BuildRule m ()
need_ = void . need

build1
  :: (Monad m, Question q)
  => q -> Build m ()
build1 q = do
  rules <- getRuleDatabase
  execBuild q rules

execBuild :: (Rule q m r) => q -> r -> Build m ()
execBuild q rule = case executeRule q rule of
  Just m -> withRule q m
  Nothing -> logBuild $ "No rule to build: " ++ show q

build
  :: (MonadIO m, Monad m, Question q)
  => q -> Build m (Answer q)
build q = do
  oracle <- getOracle
  mExistingAnswer <- lift $ Oracle.get oracle q
  mAnswer <- case mExistingAnswer of
    Just existingAnswer -> do
      mNewAnswer <- liftIO $ answer q existingAnswer
      case mNewAnswer of
        Just _ -> do
          logBuild $ "Rebuilding: " ++ show q
          return Nothing
        Nothing -> do
          logBuild $ "Already built: " ++ show q
          return (Just existingAnswer)
    Nothing -> return Nothing

  case mAnswer of
    Just ans -> return ans
    Nothing -> do
      logBuild $ "Building " ++ show q ++ "..."
      build1 q
      ans <- liftIO $ answerAnew q
      lift $ Oracle.put oracle q ans
      logBuild $ "Built " ++ show q
      return ans
