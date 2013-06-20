{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module B.RuleDatabase
  ( RuleDatabase
  , empty
  , insert
  , singleton
  , fromList
  ) where

import Data.Maybe (maybeToList)
import Data.Semigroup hiding (Any)
import Data.Typeable
import GHC.Exts (Any)
import Prelude hiding (lookup)
import Unsafe.Coerce (unsafeCoerce)

import Data.Map (Map)
import B.Question
import B.Rule

import qualified Data.Map as Map

-- RuleDatabase m = [(Question q, m ~ AnswerMonad q)
--   => (q, DynSet ((RuleSet q r, Semigroup r) => r))]
newtype RuleDatabase (m :: * -> *) = RuleDatabase
  (Map TypeRep{-q-} Any{-RuleSet m q-})

instance Show (RuleDatabase m) where
  show (RuleDatabase xs) = show $ Map.size xs

instance Monoid (RuleDatabase m) where
  mempty = RuleDatabase Map.empty
  RuleDatabase a `mappend` RuleDatabase b
    = RuleDatabase $ Map.unionWith f a b
    where
    f :: Any -> Any -> Any
    f x y = unsafeCoerce
      $ mappendRuleSets (unsafeCoerce x) (unsafeCoerce y)

instance Semigroup (RuleDatabase m)

instance (Question q, m ~ AnswerMonad q) => Rule q (RuleDatabase m) where
  queryRule q db
    = maybeToList (lookupRuleSet db) >>= queryRule q

lookupRuleSet
  :: forall m q. (Typeable q)
  => RuleDatabase m -> Maybe (RuleSet m q)
lookupRuleSet (RuleDatabase xs)
  = fmap (unsafeCoerce :: Any -> RuleSet m q)
  $ Map.lookup (typeOf (undefined :: q)) xs

data RuleSet m q where
  RuleSet :: (Question q, m ~ AnswerMonad q)
    => Map TypeRep{-r-} Any{-ARule m q r-}
    -> RuleSet m q

instance (Question q, m ~ AnswerMonad q) => Rule q (RuleSet m q) where
  queryRule q ruleSet
    = concat $ mapTo (queryRule q) ruleSet

mapTo
  :: forall b m q.
     (forall r. (Rule q r) => r -> b)
  -> RuleSet m q -> [b]
mapTo f (RuleSet xs)
  = fmap (f' . unsafeCoerce)
  $ Map.elems xs
  where
  f' :: ARule m q r -> b
  f' (ARule r) = f r

data ARule m q r where
  ARule
    :: ( Question q
       , m ~ AnswerMonad q
       , Rule q r
       , Semigroup r
       , Typeable r
       )
    => r -> ARule m q r

mappendRuleSets
  :: forall m q. RuleSet m q -> RuleSet m q -> RuleSet m q
RuleSet a `mappendRuleSets` RuleSet b
  = RuleSet $ Map.unionWith f a b
    where
    f :: Any -> Any -> Any
    f x y = unsafeCoerce $ mappendRules
      (unsafeCoerce x :: ARule m q r)
      (unsafeCoerce y :: ARule m q r)

mappendRules :: ARule m q r -> ARule m q r -> ARule m q r
ARule a `mappendRules` ARule b = ARule $ case cast a of
  Just a' -> a' <> b
  Nothing -> error "mappendRules: Type mismatch"

empty :: RuleDatabase m
empty = mempty

insert
  :: ( Question q
     , m ~ AnswerMonad q
     , Rule q r
     , Typeable r
     , Semigroup r
     )
  => r -> RuleDatabase m -> RuleDatabase m
insert x xs = singleton x <> xs

singleton
  :: forall q m r.
     ( Question q
     , m ~ AnswerMonad q
     , Rule q r
     , Typeable r
     , Semigroup r
     )
  => r -> RuleDatabase m
singleton x
  = RuleDatabase $ Map.singleton
    (typeOf (undefined :: q))
    $ unsafeCoerce $ singletonRuleSet x

singletonRuleSet
  :: forall q m r.
     ( Question q
     , m ~ AnswerMonad q
     , Rule q r
     , Typeable r
     , Semigroup r
     )
  => r -> RuleSet m q
singletonRuleSet x = RuleSet $ Map.singleton
  (typeOf (undefined :: r))
  (unsafeCoerce (ARule x))

fromList
  :: ( Question q
     , m ~ AnswerMonad q
     , Rule q r
     , Typeable r
     , Semigroup r
     )
  => [r] -> RuleDatabase m
fromList = mconcat . map singleton
