{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DynSet
  ( DynSet
  , singleton
  , insert
  , mapTo
  , lookup
  ) where

import Data.Monoid (Monoid)
import Data.Typeable
import GHC.Exts (Any, Constraint)
import Prelude hiding (lookup, map)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map

data Dictionary (c :: * -> Constraint) (a :: *) where
  Dictionary :: (c a) => Dictionary c a

data DictValue (c :: * -> Constraint) (a :: *)
  = DictValue (Dictionary c a) a

-- | A set of values unique by type.
newtype DynSet (c :: * -> Constraint)
  = DynSet (Map.Map TypeRep Any{-DictValue-})
  deriving (Monoid)

unsafeDictValueToAny :: forall a c. (c a) => a -> DictValue c a
unsafeDictValueToAny x = (DictValue (Dictionary :: Dictionary c a) (x))

-- | Constraint instances are provided.
unsafeDictValueFromAny :: Any -> DictValue c a
unsafeDictValueFromAny = unsafeCoerce

singleton
  :: forall a c. (Typeable a, c a)
  => a -> DynSet c
singleton x = DynSet $ Map.singleton key value
  where
  key = typeOf x
  value = unsafeCoerce (unsafeDictValueToAny x :: DictValue c a)

insert
  :: forall a c. (Typeable a, c a)
  => a -> DynSet c -> DynSet c
insert x (DynSet xs) = DynSet $ Map.insert key value xs
  where
  key = typeOf x
  value = unsafeCoerce (unsafeDictValueToAny x :: DictValue c a)

mapTo
  :: forall b c.
     (forall a. (c a) => a -> b)
  -> DynSet c -> [b]
mapTo f (DynSet xs)
  = fmap (f' . unsafeDictValueFromAny)
  $ Map.elems xs
  where
  f' :: forall a. DictValue c a -> b
  f' (DictValue Dictionary x) = f x

lookup
  :: forall a c. (Typeable a, c a)
  => DynSet c -> Maybe a
lookup (DynSet xs)
  = fmap f $ Map.lookup key xs
  where
  key = typeOf (undefined :: a)
  f x = let DictValue Dictionary x' = unsafeCoerce x in x'
