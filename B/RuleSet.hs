{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module B.RuleSet where

import Data.Typeable (Typeable)

import {-# SOURCE #-} B.Monad (BuildRule)
import B.Question

class (Question q, Typeable r) => RuleSet q r | r -> q where
  executeRule :: r -> q -> Maybe (BuildRule ())
