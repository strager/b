{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module B.Oracle
  ( Oracle(..)
  ) where

import B.Question

data Oracle m = Oracle
  { get :: (Question q, m ~ AnswerMonad q) => q -> m (Maybe (Answer q))
  , put :: (Question q, m ~ AnswerMonad q) => q -> Answer q -> m ()

  , recheck :: (Question q, m ~ AnswerMonad q) => q -> m ()
  , recheckAll :: m ()

  , addDependency
    :: ( Question from
       , Question to
       , m ~ AnswerMonad from
       , m ~ AnswerMonad to
       )
    => from -> to -> m ()
  }
