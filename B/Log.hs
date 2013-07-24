{-# LANGUAGE GADTs #-}

module B.Log
  ( LogMessage(..)
  , isError
  ) where

import Data.Typeable

import B.Question

data LogMessage where
  Exception :: String -> LogMessage

  Building :: (Question q) => q -> LogMessage
  Rebuilding :: (Question q) => q -> LogMessage
  AlreadyBuilt :: (Question q) => q -> LogMessage
  DoneBuilding :: (Question q) => q -> LogMessage
  BuildingDependencies
    :: (Question q)
    => q
    -> [AQuestion m]
    -> LogMessage

instance Show LogMessage where
  showsPrec _ message = case message of
    Exception ex -> shows ex
    Building q -> showString "Building " . shows q . showString "..."
    Rebuilding q -> showString "Rebuilding " . shows q . showString "..."
    AlreadyBuilt q -> showString "Already built " . shows q
    DoneBuilding q -> showString "Done building " . shows q
    BuildingDependencies from to
      -> showString "Added dependencies of " . shows from . showString ": "
      . showString (unwords (map show to))

instance Eq LogMessage where
  lhs == rhs = case (lhs, rhs) of
    (Exception    a, Exception    b) -> a == b
    (Building     a, Building     b) -> cast a == Just b
    (Rebuilding   a, Rebuilding   b) -> cast a == Just b
    (AlreadyBuilt a, AlreadyBuilt b) -> cast a == Just b
    (DoneBuilding a, DoneBuilding b) -> cast a == Just b

    (BuildingDependencies a aDeps, BuildingDependencies b bDeps)
      -> cast a == Just b
      && and (zipWith questionEquals aDeps bDeps)

    _ -> False

isError :: LogMessage -> Bool
isError (Exception _) = True
isError _ = False
