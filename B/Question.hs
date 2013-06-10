{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module B.Question
  ( Question(..)
  ) where

import Data.Typeable (Typeable)

type Value a = (Eq a, Show a, Typeable a)

class (Value q, Value (Answer q)) => Question q where
  type Answer q :: *
  answerAnew :: q -> IO (Answer q)
  answer :: q -> Answer q -> IO (Maybe (Answer q))
  answer q _oldAnswer{-FIXME Should be used-} = fmap Just $ answerAnew q
