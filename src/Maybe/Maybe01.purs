module Maybe.Maybe01 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

data Mayb3 a
  = Nothing
  | Just a

derive instance genericMayb3 :: Generic (Mayb3 a) _

instance showMayb3 :: Show a => Show (Mayb3 a) where
  show = genericShow

id :: forall a. a -> a
id x = x

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

instance functorMayb3 :: Functor Mayb3 where
  map _ Nothing = Nothing
  map f (Just x) = Just (f x)

class
  Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

instance applyMayb3 :: Apply Mayb3 where
  apply Nothing _ = Nothing
  apply (Just f) x = f `map` x

class
  Apply f <= Applicative f where
  pure :: forall a. a -> f a

instance applicativeMayb3 :: Applicative Mayb3 where
  pure = Just

class
  Applicative f <= Bind f where
  bind :: forall a b. f a -> (a -> f b) -> f b

instance bindMayb3 :: Bind Mayb3 where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

class
  Apply f <= Join f where
  join :: forall a. f (f a) -> f a

instance joinMayb3 :: Join Mayb3 where
  join f = f `bind` id

--join Nothing = Nothing
--join (Just x) = x
class (Applicative m, Join m) <= Monad m

instance monadMayb3 :: Monad Mayb3

bind' :: forall a b m. Monad m => m a -> (a -> m b) -> m b
bind' x f = join $ map f x

test :: Effect Unit
test = do
  log $ "map (_ + 1) over (Just 2):\n\t"
    <> (show $ (_ + 1) `map` Just 2)
  log $ "lift (_ + 1) into Mayb3.Just and apply to (Just 2):\n\t"
    <> (show $ Just (_ + 1) `apply` Just 2)
  log $ "lift (_ + 1) into Mayb3.Just and apply to (pure 2):\n\t"
    <> (show $ Just (_ + 1) `apply` pure 2)
  log $ "BIND:\n\t"
    <> ( show
          $ Just 2
              `bind`
                (pure <<< (_ + 1))
              `bind`
                (pure <<< (_ + 2))
      ) -- needs parentheses because operator precedence is not set for `bind`
  log $ "BIND (associative composition):\n\t"
    <> ( show
          $ Just 2
              `bind`
                ( pure
                    <<< (_ + 1)
                    >>> (_ + 2)
                )
      ) -- needs parentheses because operator precedence is not set for `bind`
  log $ "JOIN `compose` the functions and then `pure`/lift and then `map` over (Just 2) and then `join`:\n\t"
    <> ( show
          ( join
              ( map
                  ( pure
                      <<< (_ + 1)
                      >>> (_ + 2)
                  )
                  (Just 2)
              )
          )
      )
  log $ "JOIN `compose` the functions and then `pure`/lift and then `map` over (Just 2) and then `join`:\n\t"
    <> ( show
          $ join
          $ map
              ( pure
                  <<< (_ + 1)
                  >>> (_ + 2)
              )
              (Just 2)
      )
  log $ "helper bind' via JOIN:\n\t"
    <> ( show
          $ Just 2
              `bind'`
                ( pure
                    <<< (_ + 1)
                    >>> (_ + 2)
                )
      ) -- needs parentheses because operator precedence is not set for `bind`
  log $ "monad do:\n\t"
    <> show do
        x <- Just 2
        y <- pure $ x + 1
        z <- pure $ y + 2
        pure z
  log $ "monad do let:\n\t"
    <> ( show do
          x <- Just 2
          let
            y = x + 1

            z = y + 2
          pure z
      )
