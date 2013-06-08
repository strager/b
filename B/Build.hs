module B.Build
  ( build
  ) where

import qualified Control.Exception as Ex

import B.Monad
import B.Oracle (Oracle)
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.RuleSet

import qualified B.Oracle as Oracle

build1
  :: (Question q)
  => RuleDatabase
  -> q
  -> Build ()
build1 rules q = case executeRule rules q of
  Just m -> m
  Nothing -> Ex.throwIO . Ex.ErrorCall
    $ "No rule to build " ++ show q

build
  :: (Question q)
  => RuleDatabase
  -> Oracle IO
  -> q
  -> Build (Answer q)
build rules oracle q = do
  mExistingAnswer <- Oracle.get oracle q
  mAnswer <- case mExistingAnswer of
    Just existingAnswer -> do
      mNewAnswer <- answer q existingAnswer
      case mNewAnswer of
        Just _ -> do
          putStrLn $ "Rebuilding: " ++ show q
          return Nothing
        Nothing -> do
          putStrLn $ "Already built: " ++ show q
          return (Just existingAnswer)
    Nothing -> return Nothing

  case mAnswer of
    Just ans -> return ans
    Nothing -> do
      putStrLn $ "Building " ++ show q ++ "..."
      build1 rules q
      ans <- answerAnew q
      Oracle.put oracle q ans
      putStrLn $ "Built " ++ show q
      return ans
