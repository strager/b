{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module B.Rule
  ( Rule(..)

  , Rules(..)
  , singletonRules
  ) where

import Data.Semigroup (Semigroup)
import Data.Typeable (Typeable)

import {-# SOURCE #-} B.Monad (BuildRule)
import B.Question

class (Question q) => Rule q r | r -> q where
  queryRule :: q -> r -> [BuildRule (AnswerMonad q) ()]

newtype Rules q r = Rules
  [q -> [BuildRule (AnswerMonad q) ()]]
  deriving (Typeable, Semigroup)

instance (Question q, Rule q r) => Rule q (Rules q r) where
  queryRule q (Rules rules) = concatMap ($ q) rules

singletonRules :: (Rule q r) => r -> Rules q r
singletonRules r = Rules [(`queryRule` r)]
