module Utils where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

print :: forall m a. MonadEffect m => Show a => a -> m Unit
print = log <<< show
