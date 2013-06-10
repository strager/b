{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module B.Oracle where

import B.Question
import B.RuleSet

data Oracle m = Oracle
  { get :: (Question q) => q -> m (Maybe (Answer q))
  , put :: (Question q) => q -> Answer q -> m ()

  , addDependency
    :: (RuleSet from r, Question to)
    => from -> to -> r{-undefined-} -> m ()
  }
