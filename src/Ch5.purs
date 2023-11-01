module Ch5
  ( singleton
  , test
  ) where

import Prelude (Unit, (+), show, discard)
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))

-- the flip function that flips the order of the arguments of a function
flip :: forall a b c. (a -> b -> c) -> (b -> a -> c)
flip f = \x y -> f y x

-- the const function that returns as const value
const :: forall a b. a -> b -> a
const x _ = x

-- the function apply
apply :: forall a b. (a -> b) -> (a -> b)
apply f x = f x

-- symbolic alias for the apply function
infixr 0 apply as $

-- flip the arguments of the apply function, argument first then function after
applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped f x = x f

-- symbolic alias for the applyFlipped function
infixl 1 applyFlipped as #

-- creates a list with a single element
singleton :: forall a. a -> List a
singleton a = a : Nil

-- checks if a list is empty (Nil)
nullList :: forall a. List a -> Boolean
nullList Nil = true

nullList _ = false

-- the snoc function that appends an element to the end of a list
snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton x

snoc (y : ys) x = y : snoc ys x

-- the length function calculates the length of a list
length :: forall a. List a -> Int -> Int
length Nil l = l

length (_ : xs) l = length xs (l + 1)

-- the head function returns the first element of a list
head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

-- the tail function ignores the first element and returns the rest of the list
tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

-- the last function returns the last element of a list or Nothing
last :: forall a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

-- init is to last as tail is to head
init :: forall a. List a -> Maybe (List a)
init Nil = Nothing
init (x : Nil) = Just Nil
init (x : xs) = x : init xs

-- the test function
test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 2 1 # show # log
  log $ show $ singleton "xyz"
  log $ show $ nullList Nil
  log $ show $ nullList ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length (1 : 2 : 3 : Nil) 0
  log $ show $ head (Nil :: List Unit)
  log $ show $ head ("abc": "123" : Nil)
  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc": "123" : Nil)
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log $ show $ init Nil
  -- log $ show $ init (1 : Nil)
  -- log $ show $ init (1 : 2 : Nil)
  -- log $ show $ init (1 : 2 : 3 : Nil)