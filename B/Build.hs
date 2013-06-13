module B.Build
  ( build
  , need
  , need_
  ) where

import Control.Monad
import Control.Monad.Trans.Class

import B.Log
import B.Monad
import B.Question
import B.Rule

import qualified B.Oracle as Oracle

need
  :: (Monad m, Question m q)
  => q -> BuildRule m (Answer q)
need q = do
  oracle <- liftBuild getOracle
  AQuestion from <- getQuestion
  liftBuild . lift $ Oracle.addDependency oracle from q
  liftBuild $ build q

need_
  :: (Monad m, Question m q)
  => q -> BuildRule m ()
need_ = liftM (const ()) . need

build
  :: (Monad m, Question m q)
  => q -> Build m (Answer q)
build q = do
  oracle <- getOracle
  mExistingAnswer <- lift $ Oracle.get oracle q
  case mExistingAnswer of
    Just existingAnswer -> do
      logBuild $ AlreadyBuilt q
      return existingAnswer
    Nothing -> actuallyBuild q

actuallyBuild
  :: (Question m q)
  => q -> Build m (Answer q)
actuallyBuild q = do
  rules <- getRuleDatabase
  case executeRule q rules of
    Just m -> do
      logBuild $ Building q
      withRule q m
      ans <- lift $ answer q
      oracle <- getOracle
      lift $ Oracle.put oracle q ans
      logBuild $ DoneBuilding q
      return ans
    Nothing -> do
      logBuild $ NoRuleError q
      lift $ answer q
