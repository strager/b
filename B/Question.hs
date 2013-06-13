{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module B.Question
  ( Question(..)
  , AQuestion(..)
  ) where

import Data.Typeable

type Value a = (Eq a, Show a, Typeable a)

class (Monad m, Value q, Value (Answer q))
  => Question m q | q -> m where
  type Answer q :: *
  answer :: q -> m (Answer q)

data AQuestion m where
  AQuestion :: (Question m q) => q -> AQuestion m

instance Eq (AQuestion m) where
  AQuestion a == AQuestion b = cast a == Just b

instance Show (AQuestion m) where
  showsPrec prec (AQuestion q) = showsPrec prec q
