{-# LANGUAGE GADTs #-}

module B.Log
  ( LogMessage(..)
  , isError
  ) where

import B.Question

data LogMessage where
  NoRuleError :: (Question m q) => q -> LogMessage

  Building :: (Question m q) => q -> LogMessage
  Rebuilding :: (Question m q) => q -> LogMessage
  AlreadyBuilt :: (Question m q) => q -> LogMessage
  DoneBuilding :: (Question m q) => q -> LogMessage

instance Show LogMessage where
  showsPrec _ message = case message of
    NoRuleError q -> showString "No rule to build " . shows q
    Building q -> showString "Building " . shows q . showString "..."
    Rebuilding q -> showString "Rebuilding " . shows q . showString "..."
    AlreadyBuilt q -> showString "Already built " . shows q
    DoneBuilding q -> showString "Done building " . shows q

isError :: LogMessage -> Bool
isError (NoRuleError _) = True
isError _ = False
