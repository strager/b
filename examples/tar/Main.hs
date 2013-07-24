{-# LANGUAGE OverloadedStrings #-}

-- | Port of Shake's Examples.Tar.Main to B.

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable
import Data.Typeable.Internal (TypeRep(TypeRep))
import Filesystem (createTree)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import Prelude hiding (FilePath, readFile)
import System.Exit
import System.Process (rawSystem)

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as Path

import B.File
import B.Oracle.Binary
import B.Question
import B.RuleDatabase (RuleDatabase)

import qualified B.Monad as B
import qualified B.Oracle.InMemory as InMemory
import qualified B.Oracle.InMemory.Pure as OraclePure
import qualified B.RuleDatabase as RuleDatabase

-- | Reads lines of a file, assuming UTF-8.
readFileLines
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> B.BuildRule m [Text]
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

lookupQuestion
  :: (MonadIO m, Typeable1 m)
  => Fingerprint
  -> Maybe (AQuestion m{-undefined-})
lookupQuestion fingerprint = RuleDatabase.lookupQuestion
  (TypeRep fingerprint undefined undefined)
  ruleDatabase

readOracle
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> IO (OraclePure.State m)
readOracle path = decode `fmap` FS.readFile path
  where
  decode
    = Binary.runGet (OraclePure.getState lookupQuestion)
    . BSLazy.fromStrict

writeOracle :: FilePath -> OraclePure.State m -> IO ()
writeOracle path
  = FS.writeFile path
  . BSLazy.toStrict . Binary.runPut
  . OraclePure.putState

main :: IO ()
main = do
  let dbPath = root </> "build.db"
  exists <- FS.isFile dbPath
  state <- if exists
    then readOracle dbPath
    else return OraclePure.empty

  storage <- newMVar state
  let oracle = InMemory.mkMVarOracleWithStorage storage

  createTree root

  let
    logMessage x = putStrLn ("> " ++ show x)
    run = print <=< B.runBuild B.BuildEnv
      { B.ruleDatabase = ruleDatabase
      , B.oracle = oracle
      , B.logger = logMessage
      , B.latch = B.defaultLatch
      , B.parallel = B.defaultParallel
      }

  run $ buildFile (root </> "result.tar")

  state' <- takeMVar storage
  writeOracle dbPath state'
