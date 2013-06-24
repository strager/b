{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Either
-- Copyright   :  (C) 2008-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs
--
-- This module provides a minimalist 'Either' monad transformer.
--
-- Modified to reduce dependency count.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Either
  ( EitherT(..)
  , eitherT
  , bimapEitherT
  , mapEitherT
  , hoistEither
  , left
  , right
  ) where

import Control.Applicative
import Control.Monad (liftM, MonadPlus(..))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Traversable
import Data.Semigroup

-- | 'EitherT' is a version of 'Control.Monad.Trans.Error.ErrorT' that does not
-- require a spurious 'Control.Monad.Error.Class.Error' instance for the 'Left'
-- case.
--
-- 'Either' is a perfectly usable 'Monad' without such a constraint. 'ErrorT' is
-- not the generalization of the current 'Either' monad, it is something else.
--
-- This is necessary for both theoretical and practical reasons. For instance an
-- apomorphism is the generalized anamorphism for this Monad, but it cannot be
-- written with 'ErrorT'.
--
-- In addition to the combinators here, the @errors@ package provides a large 
-- number of combinators for working with this type.
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- | Given a pair of actions, one to perform in case of failure, and one to perform
-- in case of success, run an 'EitherT' and get back a monadic result.
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT m) = m >>= \z -> case z of
  Left a -> f a
  Right b -> g b
{-# INLINE eitherT #-}

-- | Analogous to 'Left'. Equivalent to 'throwError'.
left :: Monad m => e -> EitherT e m a
left = EitherT . return . Left
{-# INLINE left #-}

-- | Analogous to 'Right'. Equivalent to 'return'.
right :: Monad m => a -> EitherT e m a
right = return
{-# INLINE right #-}

-- | Map over both failure and success.
bimapEitherT :: Functor m => (e -> f) -> (a -> b) -> EitherT e m a -> EitherT f m b
bimapEitherT f g (EitherT m) = EitherT (fmap h m) where
  h (Left e)  = Left (f e)
  h (Right a) = Right (g a)
{-# INLINE bimapEitherT #-}

-- | Map the unwrapped computation using the given function.
--
-- @
-- 'runEitherT' ('mapEitherT' f m) = f ('runEitherT' m)
-- @
mapEitherT :: (m (Either e a) -> n (Either e' b)) -> EitherT e m a -> EitherT e' n b
mapEitherT f m = EitherT $ f (runEitherT m)
{-# INLINE mapEitherT #-}

-- | Lift an 'Either' into an 'EitherT'
hoistEither :: Monad m => Either e a -> EitherT e m a
hoistEither = EitherT . return
{-# INLINE hoistEither #-}

instance Monad m => Functor (EitherT e m) where
  fmap f = EitherT . liftM (fmap f) . runEitherT
  {-# INLINE fmap #-}

instance Monad m => Applicative (EitherT e m) where
  pure a  = EitherT $ return (Right a)
  {-# INLINE pure #-}
  EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
    Left  e -> return (Left e)
    Right k -> v >>= \mv -> case mv of
      Left  e -> return (Left e)
      Right x -> return (Right (k x))
  {-# INLINE (<*>) #-}

instance (Monad m, Monoid e) => Alternative (EitherT e m) where
  EitherT m <|> EitherT n = EitherT $ m >>= \a -> case a of
    Left l -> liftM (\b -> case b of
      Left l' -> Left (mappend l l')
      Right r -> Right r) n
    Right r -> return (Right r)
  {-# INLINE (<|>) #-}

  empty = EitherT $ return (Left mempty)
  {-# INLINE empty #-}

instance (Monad m, Monoid e) => MonadPlus (EitherT e m) where
  mplus = (<|>)
  {-# INLINE mplus #-}

  mzero = empty
  {-# INLINE mzero #-}

instance Monad m => Semigroup (EitherT e m a) where
  EitherT m <> EitherT n = EitherT $ m >>= \a -> case a of
    Left _  -> n
    Right r -> return (Right r)
  {-# INLINE (<>) #-}

instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  {-# INLINE return #-}
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)
  {-# INLINE (>>=) #-}
  fail = EitherT . fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (EitherT e m) where
  mfix f = EitherT $ mfix $ \a -> runEitherT $ f $ case a of
    Right r -> r
    _       -> error "empty mfix argument"
  {-# INLINE mfix #-}

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance Foldable m => Foldable (EitherT e m) where
  foldMap f = foldMap (either mempty f) . runEitherT
  {-# INLINE foldMap #-}

instance (Monad f, Traversable f) => Traversable (EitherT e f) where
  traverse f (EitherT a) =
    EitherT <$> traverse (either (pure . Left) (fmap Right . f)) a
  {-# INLINE traverse #-}
