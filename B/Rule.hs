{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module B.Rule
  ( Rule(..)

  , Rules(..)
  , singletonRules

  , traceQueryRule
  ) where

import Data.Semigroup (Semigroup)
import Data.Typeable (Typeable)

#ifdef DEBUG
import Data.Typeable (typeOf)
import Debug.Trace
#endif

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

traceQueryRule
  :: (Rule q r, Typeable r)
  => q -> r -> [BuildRule (AnswerMonad q) ()]
#ifdef DEBUG
traceQueryRule q r = trace msg $ queryRule q r
  where
  msg
    = showString "queryRule' "
    . showsPrec 11 q
    . showString " (?::"
    . shows (typeOf r)
    $ showString ")" ""
#else
traceQueryRule = queryRule
#endif
