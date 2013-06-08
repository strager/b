{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module B.Question where

import Data.Typeable (Typeable)

import B.Monad

class (Show q, Typeable q, Show (Answer q)) => Question q where
  type Answer q :: *
  answerAnew :: q -> Build (Answer q)
  answer :: q -> Answer q -> Build (Maybe (Answer q))
  answer q oldAnswer = fmap Just $ answerAnew q
