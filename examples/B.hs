{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.IO.Class
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

instance (MonadIO m) => Question m FileModTime where
  type Answer FileModTime = Posix.EpochTime
  answerAnew (FileModTime path)
    = liftM Posix.modificationTime
    . liftIO $ Posix.getFileStatus path
  answer mtime oldAnswer = do
    newAnswer <- answerAnew mtime
    return $ if newAnswer > oldAnswer
      then Just newAnswer
      else Nothing

instance (Question m q, Rule q m r) => Rule q m [r] where
  executeRule q rules = asum $ map (executeRule q) rules

newtype FunctionIO q = Function (q -> Maybe (BuildRule IO ()))
  deriving (Typeable)

instance (Question IO q) => Rule q IO (FunctionIO q) where
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
  oracle <- InMemory.mkSTMOracle

  removeFile "test"
  removeFile "some-dep"

  let logMessage x = putStrLn ("> " ++ show x)
  let run = runBuild ruleDatabase oracle logMessage
  putStrLn "Building"
  print =<< run (build (FileModTime "test"))

  putStrLn "\nBuilding again"
  print =<< run (build (FileModTime "test"))

  putStrLn "\nBuilding dep again"
  print =<< run (build (FileModTime "some-dep"))

  putStrLn "\nTouching dep"
  writeFile "some-dep" "hah!"
  Oracle.dirty oracle (FileModTime "some-dep")

  putStrLn "\nBuilding again"
  print =<< run (build (FileModTime "test"))

  putStrLn "\nBuilding dep again"
  print =<< run (build (FileModTime "some-dep"))
