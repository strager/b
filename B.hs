{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module B where

import Data.Foldable (asum)
import Data.Typeable

import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import B.Build
import B.Monad
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.RuleSet

import qualified B.Oracle.InMemory as InMemory
import qualified B.RuleDatabase as RuleDatabase

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
