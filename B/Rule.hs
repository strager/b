{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module B.Rule
  ( Rule(..)
  ) where

import {-# SOURCE #-} B.Monad (BuildRule)
import B.Question

class (Question q) => Rule q r | r -> q where
  executeRule :: q -> r -> Maybe (BuildRule (AnswerMonad q) ())
