{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module B.Latch
  ( Latch(..)
  , LatchKey(..)
  , latched
  ) where

import Data.Typeable

-- | Allows only a single concurrent evaluation of an action
-- for a given key.  While the action is being executed,
-- concurrent calls with the same key should block until the
-- first execution completes.
--
-- The action must be idempotent.  The action is not
-- required to be reentrant.
data Latch m where
  Latch :: forall m. (forall a. m a -> LatchKey -> m a) -> Latch m

data LatchKey where
  LatchKey :: (Typeable a, Eq a) => a -> LatchKey

instance Eq LatchKey where
  LatchKey a == LatchKey b
    = cast a == Just b

latched
  :: (Typeable key, Eq key)
  => Latch m -> m a -> key -> m a
latched (Latch f) action key = f action (LatchKey key)
