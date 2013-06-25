{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

#if HAS_POLYKIND_TYPEABLE
{-# LANGUAGE StandaloneDeriving #-}
#endif

module B.File
  ( FileMissing(..)

  , fileRule
  , oneFileRule
  , filterFileRule

  , buildFile

  , needFile
  , needFiles

  , readFile

#if HAS_POLYKIND_TYPEABLE
  , Typeable1  -- HACK for compatibility.
#endif
  ) where

import Control.Applicative
import Control.Exception (SomeException(..), Exception)
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary (Binary)
import Data.Maybe (maybeToList)
import Data.Semigroup
import Data.Text.Binary ()  -- FIXME Unused on Linux.
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Typeable
import Filesystem.Path.CurrentOS  -- TODO Explicit import list.
import Prelude hiding (FilePath, readFile)

import qualified Control.Exception as Ex
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as BS
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FilePath

import B.Build
import B.Monad
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.Rule

import qualified B.RuleDatabase as RuleDatabase

#if HAS_POLYKIND_TYPEABLE
type Typeable1 = Typeable
#endif

newtype FileModTime (m :: * -> *) = FileModTime FilePath
  deriving (Eq, Ord, Show)

instance Binary (FileModTime m) where
  get = fmap (FileModTime . FilePath.decode) Binary.get
  put (FileModTime path) = Binary.put $ FilePath.encode path

#if HAS_POLYKIND_TYPEABLE
deriving instance (Typeable m) => Typeable (FileModTime m)
#else
instance (Typeable1 m) => Typeable (FileModTime m) where
  typeOf fileModTime = mkTyConApp fileModTimeTyCon [typeOf1 (f fileModTime)]
    where
    f :: t m -> m a
    f = undefined

fileModTimeTyCon :: TyCon
fileModTimeTyCon = mkTyCon3 "b" "B.File" "FileModTime"
#endif

newtype TimeStamp = TimeStamp UTCTime
  deriving (Eq, Ord, Show, Typeable)

milliseconds :: (Num a) => a
milliseconds = 1000

instance Binary TimeStamp where
  get = TimeStamp . posixSecondsToUTCTime
    . (/ milliseconds) . fromIntegral
    <$> Binary.getWord64le
  put (TimeStamp time) = Binary.putWord64le
    $ round (utcTimeToPOSIXSeconds time * milliseconds)

instance (MonadIO m, Typeable1 m) => Question (FileModTime m) where
  type Answer (FileModTime m) = TimeStamp
  type AnswerMonad (FileModTime m) = m
  answer (FileModTime path) = liftIO $ do
    exists <- FS.isFile path
    if exists
      then Right . TimeStamp <$> FS.getModified path
      else return . Left . SomeException
        $ FileMissing path

data FileMissing = FileMissing FilePath
  deriving (Typeable)

instance Show FileMissing where
  show (FileMissing path) = "File missing: " ++ show path

instance Exception FileMissing

newtype FileMap m = FileMap [FilePath -> [BuildRule m ()]]

#if HAS_POLYKIND_TYPEABLE
deriving instance (Typeable m) => Typeable (FileMap m)
#else
instance (Typeable1 m) => Typeable (FileMap m) where
  typeOf fileMap = mkTyConApp fileMapTyCon [typeOf1 (f fileMap)]
    where
    f :: t m -> m a
    f = undefined

fileMapTyCon :: TyCon
fileMapTyCon = mkTyCon3 "b" "B.File" "FileMap"
#endif

instance (MonadIO m, Typeable1 m) => Rule (FileModTime m) (FileMap m) where
  queryRule (FileModTime path) (FileMap xs)
    = case concatMap ($ path) xs of
      [] -> [fallback]
      builders -> builders
    where
    fallback = do
      mModTime <- liftIO $ answer (FileModTime path)
      case mModTime of
        Left ex -> liftIO $ Ex.throwIO ex
        Right _ -> return ()

instance Semigroup (FileMap m) where
  FileMap a <> FileMap b = FileMap $ a <> b

fileRule
  :: (MonadIO m, Typeable1 m)
  => (FilePath -> Maybe (BuildRule m ()))
  -> RuleDatabase m
fileRule f = RuleDatabase.singleton
  $ FileMap [maybeToList . f]

oneFileRule
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> BuildRule m ()
  -> RuleDatabase m
oneFileRule path builder = fileRule
  $ \ p -> if p == path then Just builder else Nothing

filterFileRule
  :: (MonadIO m, Typeable1 m)
  => (FilePath -> Bool)
  -> (FilePath -> BuildRule m ())
  -> RuleDatabase m
filterFileRule f builder = fileRule
  $ \ p -> if f p then Just (builder p) else Nothing

buildFile
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> Build m ()
buildFile = build_ . FileModTime

needFile
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> BuildRule m ()
needFile = need_ . FileModTime

needFiles
  :: (MonadIO m, Typeable1 m)
  => [FilePath]
  -> BuildRule m ()
needFiles = mapM_ needFile

readFile
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> BuildRule m BS.ByteString
readFile path = do
  needFile path
  liftIO $ FS.readFile path
