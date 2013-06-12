{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
newtype RuleDatabase (m :: * -> *)
  = RuleDatabase (Map.Map TypeRep Any)
  deriving (Monoid)

-- A set of rule sets keyed by type.  The question type of
-- all rule sets must be the same.
newtype RuleSet q m = RuleSet (DynSet (Rule q m))

instance (Question m q) => Rule q m (RuleSet q m) where
  executeRule q (RuleSet dynMap)
    = asum $ DynSet.mapTo (executeRule q) dynMap

instance (Question m q) => Rule q m (RuleDatabase m) where
  executeRule q rules = lookupRS rules >>= (executeRule q)

insert
  :: (Rule q m r, Typeable r)
  => r -> RuleDatabase m -> RuleDatabase m
insert x xs = singleton x <> xs

lookupRS
  :: forall q m. (Question m q, Monad m)
  => RuleDatabase m -> Maybe (RuleSet q m)
lookupRS (RuleDatabase xs)
  = unsafeCoerce $ Map.lookup key xs
  where key = typeOf (undefined :: q)

singletonRS
  :: forall q m. (Question m q, Monad m)
  => RuleSet q m -> RuleDatabase m
singletonRS x = RuleDatabase
  $ Map.singleton key (unsafeCoerce x)
  where key = typeOf (undefined :: q)

singleton
  :: (Rule q m r, Typeable r)
  => r -> RuleDatabase m
singleton x = singletonRS (RuleSet (DynSet.singleton x))
