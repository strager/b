{-# LANGUAGE ScopedTypeVariables #-}

module B.Simple
  ( evalBuild
  , evalBuildStringPath
  ) where

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Typeable.Internal (TypeRep(TypeRep))
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath, readFile)
import System.IO (Handle, hClose)

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BSLazy
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FSPath

import B.Monad (Build)
import B.Oracle.Binary
import B.Question
import B.RuleDatabase (RuleDatabase)

import qualified B.Monad as B
import qualified B.Oracle as Oracle
import qualified B.Oracle.InMemory as InMemory
import qualified B.Oracle.InMemory.Pure as OraclePure
import qualified B.RuleDatabase as RuleDatabase

evalBuild
  :: forall a m. (MonadIO m)
  => FilePath
  -> RuleDatabase m
  -> Build m a
  -> m a
evalBuild dbPath ruleDatabase m = do
  exists <- liftIO $ FS.isFile dbPath
  mHandle <- if exists
    then liftM Just . liftIO $ FS.openFile dbPath FS.ReadMode
    else return Nothing
  state <- maybe
    (return OraclePure.empty)
    (liftIO . readState)
    mHandle

  storage <- liftIO $ newMVar state
  let oracle = InMemory.mkMVarOracleWithStorage storage
  Oracle.recheckAll oracle

  mResult <- B.runBuild B.BuildEnv
    { B.ruleDatabase = ruleDatabase
    , B.oracle = oracle
    , B.logger = B.defaultLogger
    } m

  -- Close the handle before writing to avoid problems with
  -- file locking.
  maybe (return ()) (liftIO . hClose) mHandle

  liftIO $ do
    state' <- takeMVar storage
    FS.withFile dbPath FS.WriteMode (`writeState` state')

  either (liftIO . throwIO . head) return mResult

  where
  lookupQuestion
    :: Fingerprint
    -> Maybe (AQuestion m{-undefined-})
  lookupQuestion fingerprint = RuleDatabase.lookupQuestion
    (TypeRep fingerprint undefined undefined)
    ruleDatabase

  readState :: Handle -> IO (OraclePure.State m)
  readState handle = decode `fmap` BSLazy.hGetContents handle
    where
    decode = Binary.runGet (OraclePure.getState lookupQuestion)

  writeState :: Handle -> OraclePure.State m -> IO ()
  writeState handle
    = BSLazy.hPut handle
    . Binary.runPut
    . OraclePure.putState

evalBuildStringPath
  :: (MonadIO m)
  => String
  -> RuleDatabase m
  -> Build m a
  -> m a
evalBuildStringPath dbPathString
  = evalBuild (FSPath.decodeString dbPathString)
