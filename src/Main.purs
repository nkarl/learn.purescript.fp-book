module Main where

import Prelude (Unit, show)
import Effect (Effect)
import Effect.Console (log)

flip :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

main :: Effect Unit
main = do
  log (show (flip const 1 2))
