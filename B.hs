{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module B where

import Data.Foldable (asum)
import Data.Typeable

import qualified Control.Exception as Ex
import qualified Data.Map as Map
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import B.Monad
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.RuleSet

import qualified B.RuleDatabase as RuleDatabase

type Oracle m = forall a q.
  (Question q, a ~ Answer q) => q -> m (Maybe a)

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
  :: (Question q, a ~ Answer q)
  => RuleDatabase
  -> Oracle Build
  -> q
  -> Build a
build rules oracle q = do
  mExistingAnswer <- oracle q
  case mExistingAnswer of
    Just answer -> do
      putStrLn $ "Already built: " ++ show q
      return answer
    Nothing -> do
      putStrLn $ "Building " ++ show q ++ "..."
      build1 rules q
      putStrLn $ "Built " ++ show q
      answerAnew q

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

oracle :: (Question q, a ~ Answer q) => q -> Build (Maybe a)
oracle _q = return Nothing

main :: IO ()
main = do
  answer <- build ruleDatabase oracle (FileModTime "test")
  print answer
