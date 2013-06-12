{-# LANGUAGE Rank2Types #-}

module B.Oracle
  ( Oracle(..)
  ) where

import B.Question

data Oracle m = Oracle
  { get :: (Question m q) => q -> m (Maybe (Answer q))
  , put :: (Question m q) => q -> Answer q -> m ()

  , dirty :: (Question m q) => q -> m ()

  , addDependency
    :: (Question m from, Question m to)
    => from -> to -> m ()
  }
