{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module B.Question
  ( Question(..)
  , AQuestion(..)
  , testAnswer
  ) where

import Control.Exception (SomeException)
import Data.Binary (Binary)
import Data.Typeable

type Value a = (Eq a, Binary a, Show a, Typeable a)

class (Monad (AnswerMonad q), Value q, Value (Answer q))
  => Question q where
  type Answer q :: *
  type AnswerMonad q :: * -> *
  answer :: q -> AnswerMonad q (Either SomeException (Answer q))

data AQuestion m where
  AQuestion :: (Question q, m ~ AnswerMonad q) => q -> AQuestion m

instance Eq (AQuestion m) where
  AQuestion a == AQuestion b = cast a == Just b

instance Show (AQuestion m) where
  showsPrec prec (AQuestion q) = showsPrec prec q

-- | Answers the given question and compares against the
-- given answer.  Returns 'False' upon failure.
testAnswer :: (Question q) => q -> Answer q -> AnswerMonad q Bool
testAnswer q a = do
  mAnswer <- answer q
  return $ case mAnswer of
    Left _ -> False
    Right a' -> a == a'
