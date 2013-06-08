{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module B where

import Data.Foldable (asum)
import Data.Typeable

import qualified Control.Exception as Ex
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import B.Monad
import B.Oracle (Oracle)
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.RuleSet

import qualified B.Oracle as Oracle
import qualified B.Oracle.InMemory as InMemory
import qualified B.RuleDatabase as RuleDatabase

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

--------------------------------------------------------------------------------------------------------------------------------------------

newtype FileModTime = FileModTime FilePath
  deriving (Show, Typeable)

instance Question FileModTime where
  type Answer FileModTime = Posix.EpochTime
  answerAnew (FileModTime path)
    = fmap Posix.modificationTime $ Posix.getFileStatus path
  answer mtime oldAnswer = do
    newAnswer <- answerAnew mtime
    return $ if newAnswer > oldAnswer
      then Just newAnswer
      else Nothing

instance (Question q) => RuleSet q [q -> Maybe (Build ())] where
  executeRule rules q = asum $ map ($ q) rules

putFileName :: FileModTime -> Maybe (Build ())
putFileName (FileModTime path) = Just $ writeFile path path

ruleDatabase :: RuleDatabase
ruleDatabase = RuleDatabase.singleton [putFileName]

main :: IO ()
main = do
  oracle <- InMemory.mkOracle

  putStrLn "----- BUILDING"
  print =<< build ruleDatabase oracle (FileModTime "test")

  putStrLn "\n----- BUILDING AGAIN"
  print =<< build ruleDatabase oracle (FileModTime "test")
