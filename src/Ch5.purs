module Ch5
  ( singleton
  , test
  ) where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, (+), (-), (<), (>=), (/=), (==), show, discard, negate, otherwise, type (~>))

-- f01. the flip function that flips the order of the arguments of a function
flip :: forall a b c. (a -> b -> c) -> (b -> a -> c)
flip f = \x y -> f y x

-- f02. the const function that returns as const value
const :: forall a b. a -> b -> a
const x _ = x

-- f03. the function apply
apply :: forall a b. (a -> b) -> (a -> b)
apply f x = f x

-- symbolic alias for the apply function
infixr 0 apply as $

-- f04. flip the arguments of the apply function, argument first then function after
applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped f x = x f

-- symbolic alias for the applyFlipped function
infixl 1 applyFlipped as #

-- f05. creates a list with a single element
singleton :: forall a. a -> List a
singleton a = a : Nil

-- f06. checks if a list is empty (Nil)
nullList :: forall a. List a -> Boolean
nullList Nil = true

nullList _ = false

-- f07. the snoc function that appends an element to the end of a list
snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton x

snoc (y : ys) x = y : snoc ys x

-- f08. the length function calculates the length of a list
length :: forall a. List a -> Int -> Int
length Nil l = l

length (_ : xs) l = length xs (l + 1)

-- f09. the head function returns the first element of a list
head :: forall a. List a -> Maybe a
head Nil = Nothing

head (x : _) = Just x

-- f10. the tail function ignores the first element and returns the rest of the list
tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing

tail (_ : xs) = Just xs

-- f11. the last function returns the last element of a list or Nothing
last :: forall a. List a -> Maybe a
last Nil = Nothing

last (x : Nil) = Just x

last (_ : xs) = last xs

-- f12. init is to last as tail is to head
init :: forall a. List a -> Maybe (List a)
init Nil = Nothing

init l = Just $ check l
  where
  check Nil = Nil

  check (_ : Nil) = Nil

  check (x : xs) = x : check xs

-- f13. uncons takes a list and return the head and the tail
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing

uncons (x : xs) = Just { head: x, tail: xs }

-- f14. index takes a list and an integer and returns the element at that position in the list
index :: forall a. List a -> Int -> Maybe a
index Nil _ = Nothing

index _ i
  | i < 0 = Nothing

index (x : _) 0 = Just x

index (_ : xs) i = index xs (i - 1)

infixl 8 index as !!  -- operator alias for the function index

-- f15. findIndex takes a predicate and a list and return the element that matches that predicate
findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
-- findIndex _ Nil = Nothing
findIndex predicate xs = check 0 xs
  where
  check :: Int -> List a -> Maybe Int
  check _ Nil = Nothing

  check i (y : ys) = if predicate y then Just i else check (i + 1) ys

findIndex2 :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex2 predicate = check 0
  where
  check :: Int -> List a -> Maybe Int
  check _ Nil = Nothing

  check i (y : ys) = case predicate y of
    true -> Just i
    false -> check (i + 1) ys

findIndex3 :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex3 predicate = check 0
  where
  check :: Int -> List a -> Maybe Int
  check _ Nil = Nothing

  check i (y : ys)
    | predicate y = Just i
    | otherwise = check (i + 1) ys

-- f16. takes a predicate and a list and return the last element that matches that predicate
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
-- findLastIndex _ Nil = Nothing
findLastIndex predicate xs = check Nothing 0 xs
  where
  check :: Maybe Int -> Int -> List a -> Maybe Int
  check found _ Nil = found

  check found i (y : ys) = check (if predicate y then Just i else found) (i + 1) ys

findLastIndex2 :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex2 predicate = check Nothing 0
  where
  check :: Maybe Int -> Int -> List a -> Maybe Int
  check found _ Nil = found

  check found i (y : ys) =
    check
      ( case predicate y of
          true -> Just i
          false -> found
      )
      (i + 1)
      ys

-- reverse a list
reverse :: List ~> List
reverse Nil = Nil

reverse (x : Nil) = singleton x

reverse (x : xs) = snoc (reverse xs) x

reverse2 :: List ~> List
reverse2 = build Nil
  where
  build acc Nil = acc

  build acc (x : xs) = build (x : acc) xs

-- concat a bunch of lists together as a single list preserving the order of elements
concat :: forall a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

-- the test function
test :: Effect Unit
test = do
  log $ show $ "f01"
  log $ show $ flip const 1 2
  log $ show "f02"
  flip const 2 1 # show # log
  log $ show "f05"
  log $ show $ singleton "xyz"
  log $ show "f06"
  log $ show $ nullList Nil
  log $ show $ nullList ("abc" : Nil)
  log $ show "f07"
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show "f08"
  log $ show $ length (1 : 2 : 3 : Nil) 0
  log $ show "f09"
  log $ show $ head (Nil :: List Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show "f10"
  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc" : "123" : Nil)
  log $ show "f11"
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log $ show "f12"
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show "f13"
  log $ show $ uncons (1 : Nil)
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log $ show "f14"
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil) (-99)
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show "f15"
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log $ show "f15 findIndex2"
  log $ show $ findIndex2 (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex2 (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex2 (10 /= _) (Nil :: List Int)
  log $ show "f15 findIndex3"
  log $ show $ findIndex3 (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex3 (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex3 (10 /= _) (Nil :: List Int)
  log $ show "f16"
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show "f16 findLastIndex2"
  log $ show $ findLastIndex2 (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex2 (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex2 (_ == 10) (11 : 12 : Nil)
  log $ show "f17 reverse O(N^2)"
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show "f17 reverse O(N) via accumulator"
  log $ show $ reverse2 (10 : 20 : 30 : Nil)
  log $ show "f18 concat"
  log $ show
    $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
