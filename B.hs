{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module B where

import Control.Applicative
import Control.Concurrent.STM
import Data.Foldable (asum)
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

import qualified Control.Exception as Ex
import qualified Data.Map as Map
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import B.Monad
import B.Oracle (Oracle)
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.RuleSet
import Data.DynSet (DynSet)

import qualified B.Oracle as Oracle
import qualified B.RuleDatabase as RuleDatabase
import qualified Data.DynSet as DynSet

build1
  :: (Question q)
  => RuleDatabase
  -> q
  -> Build ()
build1 rules q = case executeRule rules q of
  Just m -> m
  Nothing -> Ex.throwIO . Ex.ErrorCall
    $ "No rule to build " ++ show q

build
  :: (Question q)
  => RuleDatabase
  -> Oracle IO
  -> q
  -> Build (Answer q)
build rules oracle q = do
  mExistingAnswer <- Oracle.get oracle q
  mAnswer <- case mExistingAnswer of
    Just existingAnswer -> do
      mNewAnswer <- answer q existingAnswer
      case mNewAnswer of
        Just newAnswer -> do
          putStrLn $ "Rebuilding: " ++ show q
          return Nothing
        Nothing -> do
          putStrLn $ "Already built: " ++ show q
          return (Just existingAnswer)
    Nothing -> return Nothing

  case mAnswer of
    Just ans -> return ans
    Nothing -> do
      putStrLn $ "Building " ++ show q ++ "..."
      build1 rules q
      ans <- answerAnew q
      Oracle.put oracle q ans
      putStrLn $ "Built " ++ show q
      return ans

--------------------------------------------------------------------------------------------------------------------------------------------

newtype FileModTime = FileModTime FilePath
  deriving (Show, Typeable)

instance Question FileModTime where
  type Answer FileModTime = Posix.EpochTime
  answerAnew (FileModTime path)
    = fmap Posix.modificationTime $ Posix.getFileStatus path
  answer mtime oldAnswer = do
    newAnswer <- answerAnew mtime
    return $ if newAnswer > oldAnswer
      then Just newAnswer
      else Nothing

instance (Question q) => RuleSet q [q -> Maybe (Build ())] where
  executeRule rules q = asum $ map ($ q) rules

putFileName :: FileModTime -> Maybe (Build ())
putFileName (FileModTime path) = Just $ writeFile path path

ruleDatabase :: RuleDatabase
ruleDatabase = RuleDatabase.singleton [putFileName]

data QuestionAnswer q = QuestionAnswer
  { qaQuestion :: q
  , qaAnswer :: Answer q
  }
  deriving (Typeable)

type Storage = DynSet Typeable

mkOracleWithStorage :: TVar Storage -> Oracle IO
mkOracleWithStorage storageVar = Oracle.Oracle
  { Oracle.get = get
  , Oracle.put = put
  }

  where
    get
      :: forall q. (Question q)
      => q -> IO (Maybe (Answer q))
    get q = atomically $ do
      storage <- readTVar storageVar
      return $ qaAnswer
        <$> (DynSet.lookup storage :: Maybe (QuestionAnswer q))

    put
      :: forall q. (Question q)
      => q -> Answer q -> IO ()
    put q a = atomically
      $ modifyTVar storageVar
        (DynSet.insert (QuestionAnswer q a))


mkOracle :: IO (Oracle IO)
mkOracle = mkOracleWithStorage <$> newTVarIO DynSet.empty

-- HACK
oracle :: Oracle IO
oracle = unsafePerformIO mkOracle
{-# NOINLINE oracle #-}

main :: IO ()
main = do
  answer <- build ruleDatabase oracle (FileModTime "test")
  print answer
