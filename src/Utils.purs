module Utils where

import Prelude
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console

println :: forall m a. MonadEffect m => Show a => a -> m Unit
println = Console.log <<< show
