{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module B.RuleDatabase
  ( RuleDatabase
  , insert
  , singleton
  ) where

import Data.Foldable (asum)
import Data.Monoid hiding (Any)
import Data.Typeable
import GHC.Exts (Any)
import Prelude hiding (lookup)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map

import B.Question
import B.RuleSet
import Data.DynSet (DynSet)

import qualified Data.DynSet as DynSet

-- | A map from a question type 'q' to a value of type
-- 'RuleSets q'.
newtype RuleDatabase
  = RuleDatabase (Map.Map TypeRep Any)
  deriving (Monoid, Typeable)

-- A set of rule sets keyed by type.  The question type of
-- all rule sets must be the same.
newtype RuleSets q = RuleSets (DynSet (RuleSet q))
  deriving (Typeable)

instance (Question q) => RuleSet q (RuleSets q) where
  executeRule (RuleSets dynMap) q
    = asum $ DynSet.mapTo (`executeRule` q) dynMap

instance (Question q) => RuleSet q RuleDatabase where
  executeRule rules q = lookupRS rules >>= (`executeRule` q)

insert
  :: forall q r. (Typeable r, Question q, RuleSet q r)
  => r -> RuleDatabase -> RuleDatabase
insert x xs = singleton x <> xs

lookupRS
  :: forall q. (Question q)
  => RuleDatabase -> Maybe (RuleSets q)
lookupRS (RuleDatabase xs)
  = unsafeCoerce $ Map.lookup key xs
  where key = typeOf (undefined :: q)

singletonRS
  :: forall q. (Question q)
  => RuleSets q -> RuleDatabase
singletonRS x = RuleDatabase
  $ Map.singleton key (unsafeCoerce x)
  where key = typeOf (undefined :: q)

singleton
  :: forall q r. (Typeable r, RuleSet q r)
  => r -> RuleDatabase
singleton x = singletonRS (RuleSets (DynSet.singleton x))
