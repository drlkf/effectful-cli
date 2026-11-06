-- | Concurrency helpers built on "Effectful.Concurrent.Async".
module Effectful.CLI.Conc (
  concurrently,
  concurrently_,
  runConcurrently,
  runConcurrently_,
  mapConcurrently,
  mapConcurrently_,
  forConcurrently,
  forConcurrently_,
  batchConcurrently,
  batchConcurrently_,
  batchConcurrentlyWithIndex,
  batchConcurrentlyWithIndex_,
) where

import Data.Foldable (toList)
import Data.Traversable (mapAccumL)
import Effectful (Eff, (:>))
import Effectful.Concurrent.Async (
  Concurrent,
  pooledMapConcurrentlyN,
  pooledMapConcurrentlyN_,
 )
import Effectful.Concurrent.Async qualified as Async

-- | Run two actions concurrently and return both results.
concurrently
  :: Concurrent :> es
  => Eff es a
  -- ^ first action
  -> Eff es b
  -- ^ second action
  -> Eff es (a, b)
concurrently = Async.concurrently

-- | Like 'concurrently', discarding results.
concurrently_
  :: Concurrent :> es
  => Eff es a
  -- ^ first action
  -> Eff es b
  -- ^ second action
  -> Eff es ()
concurrently_ = Async.concurrently_

-- | Map an action over a container concurrently, without a concurrency limit.
mapConcurrently
  :: Concurrent :> es
  => Traversable t
  => (a -> Eff es b)
  -- ^ action for each element
  -> t a
  -- ^ elements
  -> Eff es (t b)
mapConcurrently = Async.mapConcurrently

-- | Like 'mapConcurrently', discarding results.
mapConcurrently_
  :: Concurrent :> es
  => Foldable f
  => (a -> Eff es b)
  -- ^ action for each element
  -> f a
  -- ^ elements
  -> Eff es ()
mapConcurrently_ = Async.mapConcurrently_

-- | 'mapConcurrently' with its arguments flipped.
forConcurrently
  :: Concurrent :> es
  => Traversable t
  => t a
  -- ^ elements
  -> (a -> Eff es b)
  -- ^ action for each element
  -> Eff es (t b)
forConcurrently = Async.forConcurrently

-- | Like 'forConcurrently', discarding results.
forConcurrently_
  :: Concurrent :> es
  => Foldable f
  => f a
  -- ^ elements
  -> (a -> Eff es b)
  -- ^ action for each element
  -> Eff es ()
forConcurrently_ = Async.forConcurrently_

-- | Run all actions concurrently and collect their results.
--
-- @since 0.1.0.0
runConcurrently
  :: Concurrent :> es
  => Traversable t
  => t (Eff es a)
  -- ^ actions
  -> Eff es (t a)
runConcurrently = mapConcurrently id

-- | Run all actions concurrently, discarding results.
--
-- @since 0.1.0.0
runConcurrently_
  :: Concurrent :> es
  => Foldable t
  => t (Eff es a)
  -- ^ actions
  -> Eff es ()
runConcurrently_ = mapConcurrently_ id

-- | Map over a list with at most @n@ concurrent actions.
--
-- @since 0.1.0.0
batchConcurrently
  :: Concurrent :> es
  => Traversable t
  => Int
  -- ^ maximum number of concurrent actions
  -> (a -> Eff es b)
  -- ^ action for each element
  -> t a
  -- ^ elements
  -> Eff es (t b)
batchConcurrently = pooledMapConcurrentlyN

-- | Like 'batchConcurrently', discarding results.
--
-- @since 0.1.0.0
batchConcurrently_
  :: Concurrent :> es
  => Foldable f
  => Int
  -- ^ maximum number of concurrent actions
  -> (a -> Eff es b)
  -- ^ action for each element
  -> f a
  -- ^ elements
  -> Eff es ()
batchConcurrently_ = pooledMapConcurrentlyN_

-- | Like 'batchConcurrently', also passing each element's 1-based index.
--
-- @since 0.1.0.0
batchConcurrentlyWithIndex
  :: Concurrent :> es
  => Traversable t
  => Int
  -- ^ maximum number of concurrent actions
  -> (Int -> a -> Eff es b)
  -- ^ action for each element
  -> t a
  -- ^ elements
  -> Eff es (t b)
batchConcurrentlyWithIndex n f =
  batchConcurrently n (uncurry f) . snd . mapAccumL (\i x -> (i + 1, (i, x))) 1

-- | Like 'batchConcurrentlyWithIndex', discarding results.
--
-- @since 0.1.0.0
batchConcurrentlyWithIndex_
  :: Concurrent :> es
  => Foldable f
  => Int
  -- ^ maximum number of concurrent actions
  -> (Int -> a -> Eff es b)
  -- ^ action for each element
  -> f a
  -- ^ elements
  -> Eff es ()
batchConcurrentlyWithIndex_ n f = batchConcurrently_ n (uncurry f) . zip [1 ..] . toList
