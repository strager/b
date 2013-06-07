{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module B.RuleSet where

import B.Monad
import B.Question

class (Question q) => RuleSet q r | r -> q where
  executeRule :: r -> q -> Maybe (Build ())
