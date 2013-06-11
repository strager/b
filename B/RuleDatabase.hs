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
import B.Rule
import Data.DynSet (DynSet)

import qualified Data.DynSet as DynSet

-- | A map from a question type 'q' to a value of type
-- 'RuleSet q'.
newtype RuleDatabase
  = RuleDatabase (Map.Map TypeRep Any)
  deriving (Monoid, Typeable)

-- A set of rule sets keyed by type.  The question type of
-- all rule sets must be the same.
newtype RuleSet q = RuleSet (DynSet (Rule q))
  deriving (Typeable)

instance (Question q) => Rule q (RuleSet q) where
  executeRule q (RuleSet dynMap)
    = asum $ DynSet.mapTo (executeRule q) dynMap

instance (Question q) => Rule q RuleDatabase where
  executeRule q rules = lookupRS rules >>= (executeRule q)

insert
  :: (Typeable r, Question q, Rule q r)
  => r -> RuleDatabase -> RuleDatabase
insert x xs = singleton x <> xs

lookupRS
  :: forall q. (Question q)
  => RuleDatabase -> Maybe (RuleSet q)
lookupRS (RuleDatabase xs)
  = unsafeCoerce $ Map.lookup key xs
  where key = typeOf (undefined :: q)

singletonRS
  :: forall q. (Question q)
  => RuleSet q -> RuleDatabase
singletonRS x = RuleDatabase
  $ Map.singleton key (unsafeCoerce x)
  where key = typeOf (undefined :: q)

singleton
  :: (Typeable r, Rule q r)
  => r -> RuleDatabase
singleton x = singletonRS (RuleSet (DynSet.singleton x))
