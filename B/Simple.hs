{-# LANGUAGE ScopedTypeVariables #-}

module B.Simple
  ( evalBuild
  , evalBuildStringPath
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM (STM)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Parallel (MonadParallel)
import Data.Typeable.Internal (TypeRep(TypeRep))
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath, readFile)
import System.IO (Handle, hClose)

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Ex
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BSLazy
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FSPath

import B.Latch
import B.Monad (Build)
import B.Oracle.Binary
import B.Question
import B.RuleDatabase (RuleDatabase)

import qualified B.Monad as B
import qualified B.Oracle as Oracle
import qualified B.Oracle.InMemory as InMemory
import qualified B.Oracle.InMemory.Pure as OraclePure
import qualified B.Parallel as Par
import qualified B.RuleDatabase as RuleDatabase

evalBuild
  :: forall a m. (MonadIO m, MonadParallel m)
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

  latch <- mkLatchIO `liftM` liftIO (STM.newTVarIO [])
  mResult <- B.runBuild B.BuildEnv
    { B.ruleDatabase = ruleDatabase
    , B.oracle = oracle
    , B.logger = B.defaultLogger
    , B.latch = latch
    , B.parallel = Par.fromMonadParallel
    } m

  -- Close the handle before writing to avoid problems with
  -- file locking.
  maybe (return ()) (liftIO . hClose) mHandle

  liftIO $ do
    state' <- takeMVar storage
    FS.withFile dbPath FS.WriteMode (`writeState` state')

  either (liftIO . Ex.throwIO . head) return mResult

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
  :: (MonadIO m, MonadParallel m)
  => String
  -> RuleDatabase m
  -> Build m a
  -> m a
evalBuildStringPath dbPathString
  = evalBuild (FSPath.decodeString dbPathString)

mkLatchIO
  :: (MonadIO m)
  => STM.TVar [(LatchKey, m ())]
  -> Latch m
mkLatchIO storage = Latch $ \ action key
  -> join . atomically' $ do
    blockers <- STM.readTVar storage
    case lookup key blockers of
      Just blocker -> return $ blocker >> action
      Nothing -> do
        var <- STM.newEmptyTMVar
        let blocker = atomically' $ STM.readTMVar var
        STM.writeTVar storage ((key, blocker) : blockers)
        return $ do
          result <- action
          atomically' $ STM.putTMVar var ()
          return result

atomically' :: (MonadIO m) => STM a -> m a
atomically' = liftIO . STM.atomically
