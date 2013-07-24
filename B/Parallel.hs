{-# LANGUAGE Rank2Types #-}

module B.Parallel
  ( Parallel(Parallel)
  , notParallel
  , fromMonadParallel

  , ap
  , liftM2
  , sequence
  , sequence_
  , mapM
  , mapM_
  ) where

import Control.Monad.Parallel (MonadParallel)
import Prelude hiding (mapM, mapM_, sequence, sequence_)

import qualified Control.Monad as Monad
import qualified Control.Monad.Parallel as MonadParallel

data Parallel m = Parallel
  { liftM2
    :: forall a b c. (a -> b -> c)
    -> m a
    -> m b
    -> m c
  }

notParallel :: (Monad m) => Parallel m
notParallel = Parallel Monad.liftM2

fromMonadParallel :: (MonadParallel m) => Parallel m
fromMonadParallel = Parallel MonadParallel.liftM2

ap
  :: (Monad m)
  => Parallel m
  -> m (a -> b) -> m a -> m b
ap par = liftM2 par id

sequence :: (Monad m) => Parallel m -> [m a] -> m [a]
sequence par = foldr
  (liftM2 par (:))
  (return [])

sequence_ :: (Monad m) => Parallel m -> [m a] -> m () 
sequence_ par = foldr
  (liftM2 par (\ _ _ -> ()))
  (return ())

mapM :: (Monad m) => Parallel m -> (a -> m b) -> [a] -> m [b]
mapM par f = sequence par . map f

mapM_ :: (Monad m) => Parallel m -> (a -> m b) -> [a] -> m ()
mapM_ par f = sequence_ par . map f
