module Traversable.Traversable where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldM :: forall a m. Monoid m => (a -> m) -> f a -> m

-- t :: Traversable Array, m :: Applicative Maybe
class
  (Functor t, Foldable t) <= Traversable t where
  -- Example: Applicative Maybe => (Int -> Maybe Int) -> Array Int -> Maybe (Array Int)
  traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  -- Example: Applicative Maybe => Array (Maybe Int) -> Maybe (Array Int)
  sequence :: forall a m. Applicative m => t (m a) -> m (t a)

test :: Effect Unit
test = do
  log $ show "placeholder"
