{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

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
  type AnswerMonad FileModTime = IO
  answer (FileModTime path)
    -- TODO Exceptions
    = liftM (Right . Posix.modificationTime)
    . liftIO $ Posix.getFileStatus path

instance (Question q, Rule q r) => Rule q [r] where
  queryRule q = concatMap (queryRule q)

newtype FunctionIO q = Function (q -> [BuildRule IO ()])
  deriving (Typeable)

instance (Question q, IO ~ AnswerMonad q) => Rule q (FunctionIO q) where
  queryRule q (Function rule) = rule q

root :: FilePath
root = "example-build-dir"

putFileName :: FileModTime -> BuildRule IO ()
putFileName (FileModTime path) = do
  when (path == root </> "test")
    $ need_ (FileModTime (root </> "some-dep"))
  liftIO $ writeFile path path

ruleDatabase :: RuleDatabase IO
ruleDatabase = RuleDatabase.singleton
  [Function (\ x -> [putFileName x])]

main :: IO ()
main = do
  oracle <- InMemory.mkSTMOracle

  createDirectoryIfMissing True root

  let
    logMessage x = putStrLn ("> " ++ show x)
    run = print <=< runBuild ruleDatabase oracle logMessage
    testBuild = do
      putStrLn "\nBuilding"
      run $ build (FileModTime (root </> "test"))
      putStrLn "\nBuilding again"
      run $ build (FileModTime (root </> "test"))
      putStrLn "\nBuilding dep again"
      run $ build (FileModTime (root </> "some-dep"))

  testBuild

  putStrLn "\nTouching dep"
  writeFile (root </> "some-dep") "hah!"
  Oracle.dirty oracle (FileModTime (root </> "some-dep"))

  testBuild
