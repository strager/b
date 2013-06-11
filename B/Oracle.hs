{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module B.Oracle
  ( Oracle(..)
  , Dependant(..)
  ) where

import B.Question
import B.Rule

data Oracle m = Oracle
  { get :: (Question q) => q -> m (Maybe (Answer q))
  , put :: (Question q) => q -> Answer q -> m ()

  , dirty :: (Question q) => q -> m ()

  , addDependency
    :: (Rule from r, Question to)
    => from -> to -> r{-undefined-} -> m ()
  }

data Dependant where
  Dependant :: (Rule q r) => q -> r -> Dependant
