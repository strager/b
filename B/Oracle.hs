{-# LANGUAGE Rank2Types #-}

module B.Oracle
  ( Oracle(..)
  ) where

import B.Question

data Oracle m = Oracle
  { get :: (Question q) => q -> m (Maybe (Answer q))
  , put :: (Question q) => q -> Answer q -> m ()

  , dirty :: (Question q) => q -> m ()

  , addDependency
    :: (Question from, Question to)
    => from -> to -> m ()
  }
