{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DynMap where

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

newtype DynMapC (c :: * -> Constraint)
  = DynMap (Map.Map TypeRep Any{-DictValue-})
  deriving (Monoid)

type DynMap = DynMapC Typeable

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

empty :: DynMapC c
empty = DynMap Map.empty

singleton
  :: forall a c f.
     ( Typeable a
     , c a
     )
  => a -> DynMapC c
singleton x = DynMap $ Map.singleton key value
  where
  key = typeOf x
  value = UNSAFE_VALUE_TO_ANY(c, a, x)

insert
  :: forall a c f.
     ( Typeable a
     , c a
     )
  => a -> DynMapC c -> DynMapC c
insert x (DynMap xs) = DynMap $ Map.insert key value xs
  where
  key = typeOf x
  value = UNSAFE_VALUE_TO_ANY(c, a, x)

internalError :: String -> String -> a
internalError functionName message = error
  $ "Data.DynMap." ++ functionName ++ ": "
  ++ "Internal error: " ++ message

adjust
  :: forall a c f.
     ( Typeable a
     , c a
     )
  => (a -> a)
  -> DynMapC c -> DynMapC c
adjust f (DynMap xs) = DynMap $ Map.adjust ff key xs
  where
  key = typeOf (undefined :: a)
  ff :: Any -> Any
  ff x = UNSAFE_VALUE_TO_ANY(c, a, f UNSAFE_VALUE_FROM_ANY(x))

mapTo
  :: forall b c f.
     ( forall a. (c a)
       => a -> b
     )
  -> DynMapC c -> [b]
mapTo f (DynMap xs)
  = fmap (f' . unsafeDictValueFromAny)
  $ Map.elems xs
  where
  f' :: forall a. DictValue c a -> b
  f' (DictValue Dictionary x) = f x

lookup
  :: forall a c f.
     ( Typeable a
     , c a
     )
  => DynMapC c -> Maybe a
lookup (DynMap xs)
  = fmap (\ x -> UNSAFE_VALUE_FROM_ANY(x))
  $ Map.lookup key xs
  where key = typeOf (undefined :: a)
