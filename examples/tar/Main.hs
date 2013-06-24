{-# LANGUAGE OverloadedStrings #-}

-- | Port of Shake's Examples.Tar.Main to B.

import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable
import Filesystem (createTree)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import Prelude hiding (FilePath, readFile)
import System.Exit
import System.Process (rawSystem)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Filesystem.Path.CurrentOS as Path

import B.File
import B.Monad
import B.RuleDatabase (RuleDatabase)

import qualified B.Oracle.InMemory as InMemory

-- | Reads lines of a file, assuming UTF-8.
readFileLines
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> BuildRule m [Text]
readFileLines
  = either (liftIO . throwIO) (return . Text.lines)
  . Text.decodeUtf8' <=< readFile

ruleDatabase :: (MonadIO m, Typeable1 m) => RuleDatabase m
ruleDatabase = mconcat
  [ oneFileRule (root </> "result.tar") $ do
    files <- liftM (map Path.fromText)
      $ readFileLines "examples/tar/list.txt"
    needFiles files
    exit <- liftIO . rawSystem "tar"
      $ ["-cf", Path.encodeString $ root </> "result.tar"]
      ++ map Path.encodeString files
    case exit of
      ExitSuccess -> return ()
      _ -> liftIO $ throwIO exit
  ]

root :: FilePath
root = "example-build-dir"

main :: IO ()
main = do
  oracle <- InMemory.mkSTMOracle
  createTree root

  let
    logMessage x = putStrLn ("> " ++ show x)
    run = print <=< runBuild ruleDatabase oracle logMessage

  run $ buildFile (root </> "result.tar")
