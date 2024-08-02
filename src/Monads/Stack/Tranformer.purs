module Monads.Stack.Transformer where

import Prelude

import Effect (Effect)
import Utils (print)

class MonadTransT t where
  lift :: forall m a. Monad m => m a -> t m a

test :: Effect Unit
test = do
  print "placeholder"
