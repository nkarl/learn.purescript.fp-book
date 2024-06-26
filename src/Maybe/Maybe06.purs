module Maybe.Maybe06 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)

{-- DATA MODEL --}
data Maybe' a
  = Nothing
  | Just a

derive instance genericMaybe' :: Generic (Maybe' a) _

derive instance eqMaybe' :: Eq a => Eq (Maybe' a)

derive instance ordMaybe' :: Ord a => Ord (Maybe' a)

instance showMaybe' :: Show a => Show (Maybe' a) where
  show = genericShow

{-- CUSTOM TYPECLASS DEFINTIONS --}
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
  join = (_ `bind` identity)

instance monadMaybe' :: Monad Maybe'

test :: Effect Unit
test = do
  log $ show $ (Just 1)
  log $ show $ (Just 1) == (Just 1)
  log $ show $ (Just 2) == (_ + 1) `map` (Just 1)
  log $ show $ (Just 2) == unit (_ + 1) `apply` (Just 1)
  log $ show $ (Just 4)
    == ( join
          $ map
              ( unit
                  <<< (_ + 1)
                  >>> (_ + 2)
              )
              (Just 1)
      )
  log $ show $ (Just 4)
    == ( (Just 1)
          `bind`
            ( unit
                <<< (_ + 1)
                >>> (_ + 2)
            )
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
          y <- unit $ (x + 1)
          let
            z = y + 2

            a = z + 3
          unit a
      )
