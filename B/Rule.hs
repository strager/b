{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module B.Rule
  ( Rule(..)
  ) where

import Data.Typeable (Typeable)

import {-# SOURCE #-} B.Monad (BuildRule)
import B.Question

class (Question q, Typeable r) => Rule q r | r -> q where
  executeRule :: q -> r -> Maybe (BuildRule ())
