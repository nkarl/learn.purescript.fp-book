module Traversable.Traversable where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldM :: forall a m. Monoid m => (a -> m) -> f a -> m

class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b) -- (Int -> Maybe Int) -> Array Int -> Maybe (Array Int)
  sequence :: forall a m. Applicative m => t (m a) -> m (t a)             -- Array (Maybe Int) -> Maybe (Array Int)

test :: Effect Unit
test = do
  log $ show "placeholder"
