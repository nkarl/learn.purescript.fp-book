module Ch7a where

import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard, ($), (==), (<), (>), (||))

-- d01 data Maybe a = Nothing | Just a
data Maybe a
  = Nothing
  | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false

-- d02 data Ord
instance ordMaybe :: Ord a => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare (Just x) (Just y) = compare x y
  compare _ Nothing = GT
  compare Nothing _ = LT

greaterThanOrEq :: forall a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = cmp == GT || cmp == EQ
  where
  cmp = compare x y

infixl 4 greaterThanOrEq as >=

lessThanOrEq :: forall a. Ord a => a -> a -> Boolean
lessThanOrEq x y = cmp == LT || cmp == EQ
  where
  cmp = compare x y

infixl 4 lessThanOrEq as <=

test :: Effect Unit
test = do
  -- i01 eqMaybe
  log $ show $ "d01 data Maybe"
  log $ show $ "-----------------------------"
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log $ show $ "d02 data Ord"
  log $ show $ "-----------------------------"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 1 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
