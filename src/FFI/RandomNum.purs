module FFI.RandomNum where

import Prelude

import Effect (Effect)
import Utils (print)

foreign import randomImpl :: Number

random :: Number
random = randomImpl

test :: Effect Unit
test = do
  print $ random
