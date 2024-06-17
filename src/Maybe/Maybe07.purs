module Maybe.Maybe07 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)

{-- DATA MODEL --}
data Maybe' a -- sum type of binary choices with very restricted outcome
  = Nothing
  | Just a

{-- DERIVATIVES --}
derive instance eqMaybe' :: Eq a => Eq (Maybe' a)

derive instance ordMaybe' :: Ord a => Ord (Maybe' a)

derive instance genericMaybe' :: Generic (Maybe' a) _

instance showMaybe' :: Show a => Show (Maybe' a) where
  show = genericShow

{-- TYPECLASS DEFINITIONS --}
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class
  Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

class
  Apply f <= Applicative f where
  unit :: forall a. a -> f a

class Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class Join m where
  join :: forall a. m (m a) -> m a

class (Applicative m, Bind m, Join m) <= Monad m

{-- TYPELCASS INSTANCE SIGNATURES --}
instance functorMaybe' :: Functor Maybe' where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe' :: Apply Maybe' where
  apply Nothing _ = Nothing
  apply (Just f) x = f `map` x

instance applicativeMaybe' :: Applicative Maybe' where
  unit = Just

instance bindMaybe' :: Bind Maybe' where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance joinMaybe' :: Join Maybe' where
  --join Nothing = Nothing
  --join (Just x)= x
  join = (_ `bind` identity)

instance monadMaybe' :: Monad Maybe'

test :: Effect Unit
test = do
  let
    x = Just 1
    y = Just 2
    z = Just 4
  log $ show $ x
  log $ show $ x == (Just 1)
  log $ show $ y == ((_ + 1) `map` (Just 1))
  log $ show $ y == (unit (_ + 1) `apply` (Just 1))
  -- JOINING
  log $ show $ z
    == ( join
          $ ( unit
                <<< (_ + 1)
                >>> (_ + 2)
            )
              `map`
                (Just 1)
      )
  -- BINDING
  log $ show $ (Just 7)
    == (Just 4)
        `bind`
          ( unit
              <<< (_ + 1)
              >>> (_ + 2)
          )
  -- MONAD DO
  log $ show $ (Just 7)
    == ( do
          a <- Just 1
          b <- unit $ a + 1
          c <- unit $ b + 2
          d <- unit $ c + 3
          unit d
      )
  -- MONAD DO
  log $ show $ (Just 7)
    == ( do
          a <- Just 1
          b <- unit $ a + 1
          let
            c = b + 2

            d = c + 3
          unit d
      )
