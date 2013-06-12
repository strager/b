{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module B.Rule
  ( Rule(..)
  ) where

import {-# SOURCE #-} B.Monad (BuildRule)
import B.Question

class (Question q, Monad m) => Rule q m r | r -> q m where
  executeRule :: q -> r -> Maybe (BuildRule m ())
