{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module B where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (asum)
import Data.Typeable
import System.Directory (removeFile)

import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import B.Build
import B.Monad
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.Rule

import qualified B.Oracle as Oracle
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

instance (Question q, Rule q m r) => Rule q m [r] where
  executeRule q rules = asum $ map (executeRule q) rules

newtype FunctionIO q = Function (q -> Maybe (BuildRule IO ()))
  deriving (Typeable)

instance (Question q) => Rule q IO (FunctionIO q) where
  executeRule q (Function rule) = rule q

putFileName :: FileModTime -> Maybe (BuildRule IO ())
putFileName (FileModTime path) = Just $ do
  when (path == "test")
    $ need_ (FileModTime "some-dep")
  liftIO $ writeFile path path

ruleDatabase :: RuleDatabase IO
ruleDatabase = RuleDatabase.singleton [Function putFileName]

main :: IO ()
main = do
  oracle <- InMemory.mkOracle

  removeFile "test"
  removeFile "some-dep"

  let run = runBuild ruleDatabase oracle putStrLn
  putStrLn "----- BUILDING"
  print =<< run (build (FileModTime "test"))

  putStrLn "\n----- BUILDING AGAIN"
  print =<< run (build (FileModTime "test"))

  putStrLn "\n----- BUILDING DEP AGAIN"
  print =<< run (build (FileModTime "some-dep"))

  putStrLn "\n----- TOUCHING DEP"
  writeFile "some-dep" "hah!"
  Oracle.dirty oracle (FileModTime "some-dep")

  putStrLn "\n----- BUILDING AGAIN"
  print =<< run (build (FileModTime "test"))

  putStrLn "\n----- BUILDING DEP AGAIN"
  print =<< run (build (FileModTime "some-dep"))
