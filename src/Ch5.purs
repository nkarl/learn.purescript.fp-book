module Ch5 (test) where

import Prelude (Unit, show, discard)
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)

flip :: forall a b c. (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

const :: forall a b. a -> b -> a
const x _ = x

apply :: forall a b. (a -> b) -> (a -> b)
apply f x = f x

infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

singleton :: forall a. a -> List a
singleton x = x : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton x

snoc (y : ys) x = y : snoc ys x

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz" -- create a list with one element
  log $ show $ null ("abc" : Nil) -- check if a list is empty
  log $ show $ snoc (1 : 2 : Nil) 3 -- add an element to the end of a list
