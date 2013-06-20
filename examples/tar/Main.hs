{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Port of Shake's Examples.Tar.Main to B's low-level API.

import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Semigroup
import Data.Typeable
import System.Directory (createDirectoryIfMissing)
import System.Exit
import System.FilePath ((</>))
import System.Process (rawSystem)

import qualified Data.Map as Map
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import B.Build
import B.Monad
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.Rule

import qualified B.Oracle.InMemory as InMemory
import qualified B.RuleDatabase as RuleDatabase

newtype FileModTime = FileModTime FilePath
  deriving (Eq, Ord, Show, Typeable)

instance Question FileModTime where
  type Answer FileModTime = Posix.EpochTime
  type AnswerMonad FileModTime = IO
  answer (FileModTime path)
    = liftM Posix.modificationTime
    . liftIO $ Posix.getFileStatus path

newtype FileMap = FileMap (Map FilePath [BuildRule IO ()])
  deriving (Typeable)

instance Rule FileModTime FileMap where
  executeRule (FileModTime path) (FileMap xs)
    = case Map.lookup path xs of
      Just builders -> case builders of
        [] -> Just fallback
        [builder] -> Just builder
        _ -> Nothing
      Nothing -> Just fallback

    where
    fallback = do
      _ <- liftIO $ answer (FileModTime path)
      return ()

instance Semigroup FileMap where
  FileMap a <> FileMap b = FileMap
    $ Map.unionWith mappend a b

fileRule :: FilePath -> BuildRule IO () -> RuleDatabase IO
fileRule path builder = RuleDatabase.singleton
  . FileMap $ Map.singleton path [builder]

needFiles :: [FilePath] -> BuildRule IO ()
needFiles = mapM_ (need_ . FileModTime)

readFileLines :: FilePath -> BuildRule IO [String]
readFileLines path = do
  needFiles [path]
  liftM lines . liftIO $ readFile path

ruleDatabase :: RuleDatabase IO
ruleDatabase = mconcat
  [ fileRule (root </> "result.tar") $ do
    files <- readFileLines "examples/tar/list.txt"
    needFiles files
    exit <- liftIO . rawSystem "tar"
      $ ["-cf", root </> "result.tar"] ++ files
    case exit of
      ExitSuccess -> return ()
      _ -> liftIO $ throwIO exit
  ]

root :: FilePath
root = "example-build-dir"

main :: IO ()
main = do
  oracle <- InMemory.mkSTMOracle
  createDirectoryIfMissing True root

  let
    logMessage x = putStrLn ("> " ++ show x)
    run = print <=< runBuild ruleDatabase oracle logMessage

  run $ build (FileModTime (root </> "result.tar"))
