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

import B.Oracle (Dependant(..), Oracle)
import B.Question
import B.Rule

import qualified B.Oracle as Oracle

data QuestionAnswer where
  QuestionAnswer
    :: (Question q)
    => q -> Answer q -> QuestionAnswer

data Dependency where
  Dependency
    :: (Rule from r, Question to)
    => from -> to -> r -> Dependency

mkOracle :: IO (Oracle IO)
mkOracle = mkOracleWithStorage
  <$> newTVarIO []
  <*> newTVarIO []

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f = Maybe.listToMaybe . Maybe.mapMaybe f

mkOracleWithStorage
  :: TVar [QuestionAnswer]
  -> TVar [Dependency]
  -> Oracle IO
mkOracleWithStorage qaStorage depStorage = Oracle.Oracle
  { Oracle.get = get
  , Oracle.put = put
  , Oracle.dirty = atomically . dirty
  , Oracle.addDependency = addDependency
  }

  where
    get
      :: (Question q) => q -> IO (Maybe (Answer q))
    get q
      = fmap (findJust f)
      $ readTVarIO qaStorage
      where
      f (QuestionAnswer q' a)
        | cast q == Just q' = cast a
      f _ = Nothing

    put
      :: (Question q) => q -> Answer q -> IO ()
    put q a = atomically
      $ modifyTVar qaStorage (QuestionAnswer q a :)

    dirty
      :: (Question q) => q -> STM ()
    dirty q = do
      modifyTVar qaStorage . filter
        $ \ (QuestionAnswer q' _) -> Just q /= cast q'

      deps <- readTVar depStorage
      let (deps', dependants) = f deps
      writeTVar depStorage deps'
      forM_ dependants $ \ (Dependant q' _) -> dirty q'

      where
      f :: [Dependency] -> ([Dependency], [Dependant])
      f = Either.partitionEithers . map
        (\ dep@(Dependency from to r)
          -> if cast q == Just to
            then Right $ Dependant from r
            else Left dep)

    addDependency
      :: (Rule from r, Question to)
      => from -> to -> r -> IO ()
    addDependency from to rule = atomically
      $ modifyTVar depStorage (Dependency from to rule :)
