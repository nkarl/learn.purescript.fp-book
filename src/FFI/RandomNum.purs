module FFI.RandomNum where

import Prelude

import Effect (Effect)
import Utils (println)

foreign import randomImpl :: Number

random :: Number
random = randomImpl

test :: Effect Unit
test = do
  println $ random
