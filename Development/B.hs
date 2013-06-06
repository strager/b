{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.B where

import Data.Foldable (asum)
import Data.Typeable (Typeable)

import qualified Control.Exception as Ex
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import Data.DynMap (DynMapC)

import qualified Data.DynMap as DynMap

type Build = IO

class (Show q, Typeable q) => Question q where
  type Answer q :: *
  answerAnew :: q -> Build (Answer q)
  answer :: q -> Answer q -> Build (Maybe (Answer q))
  answer q oldAnswer = fmap Just $ answerAnew q

class (Question q, q ~ RuleSetQ r) => RuleSet q r where
  type RuleSetQ r :: *
  executeRule :: r -> RuleSetQ r -> Maybe (Build ())

-- {(RuleSet r) => r}
newtype RuleSets q = RuleSets (DynMapC (RuleSet q))
  deriving (Typeable)

instance (Question q) => RuleSet q (RuleSets q) where
  type RuleSetQ (RuleSets q) = q
  executeRule (RuleSets dynMap) q
    = (asum $ DynMap.mapTo (`executeRule` q) dynMap)
      :: Maybe (Build ())

--data AnyRuleSets where
  --AnyRuleSets :: RuleSets q -> RuleDatabase
class (Question (AnyRuleSetsCQ r)) => AnyRuleSetsC r where
  type AnyRuleSetsCQ r :: *
  toRuleSets :: r -> RuleSets (AnyRuleSetsCQ r)
instance (Question q) => AnyRuleSetsC (RuleSets q) where
  type AnyRuleSetsCQ (RuleSets q) = q
  toRuleSets = id

-- Question r -> RuleSets (RuleSetQ r)
data RuleDatabase where
  RuleDatabase :: forall q. DynMapC (AnyRuleSetsC) -> RuleDatabase

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
  type RuleSetQ [q -> Maybe (Build ())] = q
  executeRule rules q = asum $ map ($ q) rules

putFileName :: FileModTime -> Maybe (Build ())
putFileName (FileModTime path) = Just $ writeFile path path

getRuleSets
  :: (Question q)
  => RuleDatabase -> Maybe (RuleSets q)
getRuleSets (RuleDatabase dynMap) = DynMap.lookup dynMap

type Oracle m = forall a q.
  (Question q, a ~ Answer q) => q -> m (Maybe a)

build1
  :: (Question q)
  => RuleDatabase
  -> q
  -> Build ()
build1 rules q = case getRuleSets rules of
  Just ruleSets -> case executeRule ruleSets q of
    Just m -> m
    Nothing -> Ex.throwIO . Ex.ErrorCall
      $ "No rule in rule set to build " ++ show q
  Nothing -> Ex.throwIO . Ex.ErrorCall
    $ "No rule sets to build " ++ show q

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
      return undefined  -- TODO

ruleSets :: RuleSets FileModTime
ruleSets = RuleSets $ DynMap.singleton [putFileName]

ruleDatabase :: RuleDatabase
ruleDatabase = RuleDatabase $ DynMap.singleton ruleSets
