module B.Build
  ( build
  , need
  , need_
  ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import qualified Control.Exception as Ex

import B.Monad
import B.Question
import B.Rule

import qualified B.Oracle as Oracle

need
  :: (Question q)
  => q -> BuildRule (Answer q)
need q = do
  oracle <- liftBuild getOracle
  AQuestion from <- getQuestion
  liftIO $ Oracle.addDependency oracle from q
  liftBuild $ build q

need_
  :: (Question q)
  => q -> BuildRule ()
need_ = void . need

build1
  :: (Question q)
  => q -> Build ()
build1 q = do
  rules <- getRuleDatabase
  execBuild q rules

execBuild :: (Rule q r) => q -> r -> Build ()
execBuild q rule = case executeRule q rule of
  Just m -> withRule q m
  Nothing -> liftIO . Ex.throwIO . Ex.ErrorCall
    $ "No rule to build " ++ show q

build
  :: (Question q)
  => q -> Build (Answer q)
build q = do
  oracle <- getOracle
  mExistingAnswer <- liftIO $ Oracle.get oracle q
  mAnswer <- case mExistingAnswer of
    Just existingAnswer -> do
      mNewAnswer <- liftIO $ answer q existingAnswer
      case mNewAnswer of
        Just _ -> do
          liftIO . putStrLn $ "Rebuilding: " ++ show q
          return Nothing
        Nothing -> do
          liftIO . putStrLn $ "Already built: " ++ show q
          return (Just existingAnswer)
    Nothing -> return Nothing

  case mAnswer of
    Just ans -> return ans
    Nothing -> do
      liftIO . putStrLn $ "Building " ++ show q ++ "..."
      build1 q
      ans <- liftIO $ answerAnew q
      liftIO $ Oracle.put oracle q ans
      liftIO . putStrLn $ "Built " ++ show q
      return ans
