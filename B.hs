{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module B where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
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
  deriving (Eq, Show, Typeable)

instance Question FileModTime where
  type Answer FileModTime = Posix.EpochTime
  answerAnew (FileModTime path)
    = fmap Posix.modificationTime $ Posix.getFileStatus path
  answer mtime oldAnswer = do
    newAnswer <- answerAnew mtime
    return $ if newAnswer > oldAnswer
      then Just newAnswer
      else Nothing

instance (Question q) => RuleSet q [q -> Maybe (BuildRule ())] where
  executeRule rules q = asum $ map ($ q) rules

putFileName :: FileModTime -> Maybe (BuildRule ())
putFileName (FileModTime path) = Just $ do
  when (path == "test")
    $ need_ (FileModTime "some-dep")
  liftIO $ writeFile path path

ruleDatabase :: RuleDatabase
ruleDatabase = RuleDatabase.singleton [putFileName]

main :: IO ()
main = do
  oracle <- InMemory.mkOracle

  putStrLn "----- BUILDING"
  print =<< runBuild ruleDatabase oracle
    (build (FileModTime "test"))

  putStrLn "\n----- BUILDING AGAIN"
  print =<< runBuild ruleDatabase oracle
    (build (FileModTime "test"))

  putStrLn "\n----- BUILDING DEP AGAIN"
  print =<< runBuild ruleDatabase oracle
    (build (FileModTime "some-dep"))
