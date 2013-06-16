{-# LANGUAGE GADTs #-}

module B.Log
  ( LogMessage(..)
  , isError
  ) where

import Data.Typeable

import B.Question

data LogMessage where
  NoRuleError :: (Question q) => q -> LogMessage

  Building :: (Question q) => q -> LogMessage
  Rebuilding :: (Question q) => q -> LogMessage
  AlreadyBuilt :: (Question q) => q -> LogMessage
  DoneBuilding :: (Question q) => q -> LogMessage

instance Show LogMessage where
  showsPrec _ message = case message of
    NoRuleError q -> showString "No rule to build " . shows q
    Building q -> showString "Building " . shows q . showString "..."
    Rebuilding q -> showString "Rebuilding " . shows q . showString "..."
    AlreadyBuilt q -> showString "Already built " . shows q
    DoneBuilding q -> showString "Done building " . shows q

instance Eq LogMessage where
  lhs == rhs = case (lhs, rhs) of
    (NoRuleError  a, NoRuleError  b) -> cast a == Just b
    (Building     a, Building     b) -> cast a == Just b
    (Rebuilding   a, Rebuilding   b) -> cast a == Just b
    (AlreadyBuilt a, AlreadyBuilt b) -> cast a == Just b
    (DoneBuilding a, DoneBuilding b) -> cast a == Just b
    _ -> False

isError :: LogMessage -> Bool
isError (NoRuleError _) = True
isError _ = False
