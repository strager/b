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
  , buildFile
  , needFile
  , needFiles

#if HAS_POLYKIND_TYPEABLE
  , Typeable1  -- HACK for compatibility.
#endif
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

#if HAS_POLYKIND_TYPEABLE
type Typeable1 = Typeable
#endif

newtype FileModTime (m :: * -> *) = FileModTime FilePath
  deriving (Eq, Ord, Show)

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

instance (MonadIO m, Typeable1 m) => Question (FileModTime m) where
  type Answer (FileModTime m) = Posix.EpochTime
  type AnswerMonad (FileModTime m) = m
  answer (FileModTime path) = liftIO $ do
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

newtype FileMap m = FileMap (Map FilePath [BuildRule m ()])

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

instance Semigroup (FileMap m) where
  FileMap a <> FileMap b = FileMap
    $ Map.unionWith mappend a b

fileRule
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> BuildRule m ()
  -> RuleDatabase m
fileRule path builder = RuleDatabase.singleton
  . FileMap $ Map.singleton path [builder]

buildFile :: (MonadIO m, Typeable1 m) => FilePath -> Build m ()
buildFile = build_ . FileModTime

needFile :: (MonadIO m, Typeable1 m) => FilePath -> BuildRule m ()
needFile = need_ . FileModTime

needFiles :: (MonadIO m, Typeable1 m) => [FilePath] -> BuildRule m ()
needFiles = mapM_ needFile
