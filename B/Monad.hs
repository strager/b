{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module B.Monad
  ( Build
  , BuildEnv(..)
  , BuildRule

  , defaultLogger
  , defaultLatch
  , defaultParallel

  , runBuild
  , withRule
  , getRuleDatabase
  , getOracle
  , getBuildRuleParallel
  , getQuestion
  , latchBuild
  , liftBuild
  , logBuild
  , logRule
  , throwBuild
  , throwsBuild
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Data.Monoid (mappend)
import Data.Typeable (Typeable)

import qualified Control.Exception as Ex

import B.Latch
import B.Log (LogMessage)
import B.Oracle
import B.Question
import B.RuleDatabase

import qualified B.Parallel as Par

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

  , latch :: Latch m
  -- ^ Controls concurrent builds.

  , parallel :: Par.Parallel m
  -- ^ Executes actions in parallel.
  }

newtype BuildRule m a = BuildRule (ReaderT (AQuestion m) (Build m) a)
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  )

instance MonadTrans BuildRule where
  lift = liftBuild . lift

defaultLogger :: (Monad m) => LogMessage -> m ()
defaultLogger _message = return ()

defaultLatch :: Latch m
defaultLatch = Latch $ \ action _key -> action
{-# ANN defaultLatch "HLint: ignore Use const" #-}

defaultParallel :: (Monad m) => Par.Parallel m
defaultParallel = Par.notParallel

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

liftM2Either
  :: (e -> e -> e)
  -> (a -> b -> c)
  -> Either e a
  -> Either e b
  -> Either e c
liftM2Either f _ (Left a) (Left b) = Left $ f a b
liftM2Either _ _ (Left a) (Right _) = Left a
liftM2Either _ _ (Right _) (Left b) = Left b
liftM2Either _ f (Right a) (Right b) = Right $ f a b

getBuildRuleParallel :: (Monad m) => BuildRule m (Par.Parallel (BuildRule m))
getBuildRuleParallel = do
  rawPar <- liftBuild . Build $ asks parallel
  return Par.Parallel
    { Par.liftM2 = \ f ma mb -> do
      -- FIXME Duplicated (latchBuild).
      env <- liftBuild $ Build ask
      AQuestion q <- BuildRule ask
      r <- lift $ Par.liftM2 rawPar
        (liftM2Either mappend f)
        (runBuild env $ withRule q ma)
        (runBuild env $ withRule q mb)
      either (liftBuild . throwsBuild) return r
    }

getQuestion :: (Monad m) => BuildRule m (AQuestion m)
getQuestion = BuildRule ask

latchBuild
  :: (Monad m, Typeable key, Eq key)
  => Build m a -> key -> Build m a
latchBuild action key = do
  env <- Build ask
  mResult <- lift $ latched (latch env) (runBuild env action) key
  either throwsBuild return mResult

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
throwBuild ex = throwsBuild [Ex.toException ex]

throwsBuild
  :: (Monad m)
  => [Ex.SomeException] -> Build m a
throwsBuild exs = Build . lift $ left exs
