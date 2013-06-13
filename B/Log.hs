{-# LANGUAGE GADTs #-}

module B.Log
  ( LogMessage(..)
  , isError
  ) where

import Data.Typeable

import B.Question

data LogMessage m where
  NoRuleError :: (Question m q) => q -> LogMessage m

  Building :: (Question m q) => q -> LogMessage m
  Rebuilding :: (Question m q) => q -> LogMessage m
  AlreadyBuilt :: (Question m q) => q -> LogMessage m
  DoneBuilding :: (Question m q) => q -> LogMessage m

instance Show (LogMessage m) where
  showsPrec _ message = case message of
    NoRuleError q -> showString "No rule to build " . shows q
    Building q -> showString "Building " . shows q . showString "..."
    Rebuilding q -> showString "Rebuilding " . shows q . showString "..."
    AlreadyBuilt q -> showString "Already built " . shows q
    DoneBuilding q -> showString "Done building " . shows q

instance Eq (LogMessage m) where
  lhs == rhs = case (lhs, rhs) of
    (NoRuleError  a, NoRuleError  b) -> cast a == Just b
    (Building     a, Building     b) -> cast a == Just b
    (Rebuilding   a, Rebuilding   b) -> cast a == Just b
    (AlreadyBuilt a, AlreadyBuilt b) -> cast a == Just b
    (DoneBuilding a, DoneBuilding b) -> cast a == Just b
    _ -> False

isError :: LogMessage m -> Bool
isError (NoRuleError _) = True
isError _ = False
