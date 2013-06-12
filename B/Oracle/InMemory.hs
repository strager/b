{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module B.Oracle.InMemory
  ( mkOracle
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.Typeable

import qualified Data.Either as Either
import qualified Data.Maybe as Maybe

import B.Oracle (Oracle)
import B.Question

import qualified B.Oracle as Oracle

data QuestionAnswer m where
  QuestionAnswer
    :: (Question m q)
    => q -> Answer q -> QuestionAnswer m

data Dependency m where
  Dependency
    :: (Question m from, Question m to)
    => from -> to -> Dependency m

mkOracle :: IO (Oracle IO)
mkOracle = mkOracleWithStorage
  <$> newTVarIO []
  <*> newTVarIO []

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f = Maybe.listToMaybe . Maybe.mapMaybe f

editTVar :: (a -> (a, b)) -> TVar a -> STM b
editTVar f var = do
  x <- readTVar var
  let (x', ret) = f x
  writeTVar var x'
  return ret

dropDependants
  :: (Question m q)
  => q -> [Dependency m] -> ([Dependency m], [AQuestion m])
dropDependants q = Either.partitionEithers . map f
  where
  f (Dependency from to)
    | cast q == Just to
    = Right $ AQuestion from
  f dep = Left dep

mkOracleWithStorage
  :: TVar [QuestionAnswer IO]
  -> TVar [Dependency IO]
  -> Oracle IO
mkOracleWithStorage qaStorage depStorage = Oracle.Oracle
  { Oracle.get = get
  , Oracle.put = put
  , Oracle.dirty = atomically . dirty
  , Oracle.addDependency = addDependency
  }

  where
    get :: (Question IO q) => q -> IO (Maybe (Answer q))
    get q
      = fmap (findJust f)
      $ readTVarIO qaStorage
      where
      f (QuestionAnswer q' a)
        | cast q == Just q' = cast a
      f _ = Nothing

    put :: (Question IO q) => q -> Answer q -> IO ()
    put q a = atomically
      $ modifyTVar qaStorage (QuestionAnswer q a :)

    dirty :: (Question IO q) => q -> STM ()
    dirty q = do
      modifyTVar qaStorage . filter
        $ \ (QuestionAnswer q' _) -> Just q /= cast q'
      dependants <- editTVar (dropDependants q) depStorage
      forM_ dependants $ \ (AQuestion q') -> dirty q'

    addDependency
      :: (Question IO from, Question IO to)
      => from -> to -> IO ()
    addDependency from to = atomically
      $ modifyTVar depStorage (Dependency from to :)
