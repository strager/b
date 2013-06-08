{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module B.Oracle where

import B.Question

data Oracle m = Oracle
  { get :: (Question q) => q -> m (Maybe (Answer q))
  , put :: (Question q) => q -> Answer q -> m ()
  }
