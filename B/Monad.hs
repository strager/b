{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module B.Monad
  ( Build
  , BuildEnv(..)
  , BuildRule

  , defaultLogger
  , runBuild
  , withRule
  , getRuleDatabase
  , getOracle
  , getQuestion
  , liftBuild
  , logBuild
  , logRule
  , throwBuild
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import qualified Control.Exception as Ex

import B.Log (LogMessage)
import B.Oracle
import B.Question
import B.RuleDatabase

newtype Build m a = Build
  (ReaderT (BuildEnv m) (EitherT [Ex.SomeException] m) a)
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  )

instance MonadTrans Build where
  lift = Build . lift . lift

data BuildEnv m = BuildEnv
  { ruleDatabase :: RuleDatabase m
  -- ^ All rules useable for building.

  , oracle :: Oracle m
  -- ^ The source of answers to 'Question's.

  , logger :: LogMessage -> m ()
  -- ^ Records diagnostics (or not).
  }

newtype BuildRule m a = BuildRule (ReaderT (AQuestion m) (Build m) a)
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  )

defaultLogger :: (Monad m) => LogMessage -> m ()
defaultLogger _message = return ()

runBuild
  :: BuildEnv m
  -> Build m a
  -> m (Either [Ex.SomeException] a)
runBuild buildEnv (Build m)
  = runEitherT $ runReaderT m buildEnv

withRule
  :: (Question q, m ~ AnswerMonad q)
  => q -> BuildRule m a -> Build m a
withRule q (BuildRule m) = runReaderT m $ AQuestion q

getRuleDatabase :: (Monad m) => Build m (RuleDatabase m)
getRuleDatabase = Build $ asks ruleDatabase

getOracle :: (Monad m) => Build m (Oracle m)
getOracle = Build $ asks oracle

getQuestion :: (Monad m) => BuildRule m (AQuestion m)
getQuestion = BuildRule ask

liftBuild :: (Monad m) => Build m a -> BuildRule m a
liftBuild m = BuildRule $ lift m

logBuild :: (Monad m) => LogMessage -> Build m ()
logBuild message = do
  l <- Build $ asks logger
  lift $ l message

logRule :: (Monad m) => LogMessage -> BuildRule m ()
logRule = liftBuild . logBuild

throwBuild
  :: (Ex.Exception ex, Monad m)
  => ex -> Build m a
throwBuild ex
  = Build . lift
  $ left [Ex.toException ex]
