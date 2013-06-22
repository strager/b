{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module B.File
  ( FileMissing(..)
  , fileRule
  , buildFile
  , needFile
  , needFiles
  ) where

import Control.Exception (SomeException(..), Exception)
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Semigroup
import Data.Typeable
import System.Directory (doesFileExist)

import qualified Control.Exception as Ex
import qualified Data.Map as Map
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import B.Build
import B.Monad
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.Rule

import qualified B.RuleDatabase as RuleDatabase

newtype FileModTime = FileModTime FilePath
  deriving (Eq, Ord, Show, Typeable)

instance Question FileModTime where
  type Answer FileModTime = Posix.EpochTime
  type AnswerMonad FileModTime = IO
  answer (FileModTime path) = do
    exists <- doesFileExist path
    if exists
      then liftM (Right . Posix.modificationTime)
        $ Posix.getFileStatus path
      else return . Left . SomeException
        $ FileMissing path

data FileMissing = FileMissing FilePath
  deriving (Typeable)

instance Show FileMissing where
  show (FileMissing path) = "File missing: " ++ show path

instance Exception FileMissing

newtype FileMap = FileMap (Map FilePath [BuildRule IO ()])
  deriving (Typeable)

instance Rule FileModTime FileMap where
  queryRule (FileModTime path) (FileMap xs)
    = case Map.lookup path xs of
      Just builders -> case builders of
        [] -> [fallback]
        _ -> builders
      Nothing -> [fallback]

    where
    fallback = do
      mModTime <- liftIO $ answer (FileModTime path)
      case mModTime of
        Left ex -> liftIO $ Ex.throwIO ex
        Right _ -> return ()

instance Semigroup FileMap where
  FileMap a <> FileMap b = FileMap
    $ Map.unionWith mappend a b

fileRule :: FilePath -> BuildRule IO () -> RuleDatabase IO
fileRule path builder = RuleDatabase.singleton
  . FileMap $ Map.singleton path [builder]

buildFile :: FilePath -> Build IO ()
buildFile = build_ . FileModTime

needFile :: FilePath -> BuildRule IO ()
needFile = need_ . FileModTime

needFiles :: [FilePath] -> BuildRule IO ()
needFiles = mapM_ needFile
