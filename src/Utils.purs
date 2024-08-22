module Utils where

import Prelude
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console


print :: forall m a. MonadEffect m => Show a => a -> m Unit
print = Console.log <<< show
