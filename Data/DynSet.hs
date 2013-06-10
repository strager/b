{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DynSet where

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

-- | UNSAFE_VALUE_TO_ANY :: forall a c. (c a) => a -> Any
#define UNSAFE_VALUE_TO_ANY(c, a, x) \
  (unsafeCoerce (DictValue (Dictionary :: Dictionary c a) (x)))

-- | Constraint instances come from outside.
-- | UNSAFE_VALUE_FROM_ANY :: (c a) => Any -> a
#define UNSAFE_VALUE_FROM_ANY(x) \
  (let DictValue Dictionary __dynmap_a = unsafeCoerce (x) in __dynmap_a)

-- | Constraint instances are provided.
unsafeDictValueFromAny :: Any -> DictValue c a
unsafeDictValueFromAny = unsafeCoerce

empty :: DynSet c
empty = DynSet Map.empty

singleton
  :: forall a c.
     ( Typeable a
     , c a
     )
  => a -> DynSet c
singleton x = DynSet $ Map.singleton key value
  where
  key = typeOf x
  value = UNSAFE_VALUE_TO_ANY(c, a, x)

insert
  :: forall a c.
     ( Typeable a
     , c a
     )
  => a -> DynSet c -> DynSet c
insert x (DynSet xs) = DynSet $ Map.insert key value xs
  where
  key = typeOf x
  value = UNSAFE_VALUE_TO_ANY(c, a, x)

adjust
  :: forall a c.
     ( Typeable a
     , c a
     )
  => (a -> a)
  -> DynSet c -> DynSet c
adjust f (DynSet xs) = DynSet $ Map.adjust ff key xs
  where
  key = typeOf (undefined :: a)
  ff :: Any -> Any
  ff x = UNSAFE_VALUE_TO_ANY(c, a, f UNSAFE_VALUE_FROM_ANY(x))

mapTo
  :: forall b c.
     ( forall a. (c a)
       => a -> b
     )
  -> DynSet c -> [b]
mapTo f (DynSet xs)
  = fmap (f' . unsafeDictValueFromAny)
  $ Map.elems xs
  where
  f' :: forall a. DictValue c a -> b
  f' (DictValue Dictionary x) = f x

lookup
  :: forall a c.
     ( Typeable a
     , c a
     )
  => DynSet c -> Maybe a
lookup (DynSet xs)
  = fmap (\ x -> UNSAFE_VALUE_FROM_ANY(x))
  $ Map.lookup key xs
  where key = typeOf (undefined :: a)
