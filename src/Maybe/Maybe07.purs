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
  Functor p <= Apply p where
  apply :: forall a b. p (a -> b) -> p a -> p b

class
  Apply p <= Applicative p where
  unit :: forall a. a -> p a

class Bind p where
  bind :: forall a b. p a -> (a -> p b) -> p b

class Join p where
  join :: forall a. p (p a) -> p a

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
  log $ show $ (Just 1)
  log $ show $ (Just 1) == (Just 1)
  log $ show $ (Just 2) == ((_ + 1) `map` (Just 1))
  log $ show $ (Just 2) == (unit (_ + 1) `apply` (Just 1))
  log $ show $ (Just 4)
    == ( join
          $ ( unit
                <<< (_ + 1)
                >>> (_ + 2)
            )
              `map`
                (Just 1)
      )
  log $ show $ (Just 7)
    == (Just 4)
        `bind`
          ( unit
              <<< (_ + 1)
              >>> (_ + 2)
          )
  log $ show $ (Just 7)
    == ( do
          x <- Just 1
          y <- unit $ x + 1
          z <- unit $ y + 2
          a <- unit $ z + 3
          unit a
      )
  log $ show $ (Just 7)
    == ( do
          x <- Just 1
          y <- unit $ x + 1
          let
            z = y + 2

            a = z + 3
          unit a
      )
