module Maybe.Maybe03 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

data Maybe' a
  = Nothing
  | Just a

derive instance eqMaybe' :: Eq a => Eq (Maybe' a)

derive instance genericMaybe' :: Generic (Maybe' a) _

instance showMaybe' :: Show a => Show (Maybe' a) where
  show = genericShow

{--
  TYPECLASS DEFINITIONS
--}
class Join p where
  join :: forall a. p (p a) -> p a

class (Applicative m, Join m) <= Monad m

{--
  TYPECLASS INSTANCE SIGNATURES
--}
instance functorMaybe' :: Functor Maybe' where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe' :: Apply Maybe' where
  apply Nothing _ = Nothing
  apply (Just f) x = f `map` x

instance applicativeMaybe' :: Applicative Maybe' where
  pure = Just

instance bindMaybe' :: Bind Maybe' where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance joinMaybe' :: Join Maybe' where
  --join Nothing = Nothing
  --join (Just x) = x
  join = (_ `bind` id)

instance monadMaybe' :: Monad Maybe'

{--
  HELPER FUNCTIONS
--}
id :: forall a. a -> a
id x = x

test :: Effect Unit
test = do
  let
    title = "title : "

    expect = "expect: "
  log $ title <> "bind (_ + 1) and (_ + 2) to Just 1"
  log $ expect
    <> show do
        let
          x =
            Just 1
              `bind`
                ( pure
                    <<< (_ + 1)
                    >>> (_ + 2)
                )
        x == Just 4
  log $ title <> "map and join (_ + 1) and (_ + 2) to Just 1"
  log $ expect <> "(Just 4)\t"
    <> show do
        let
          x =
            join
              $ map
                  ( pure
                      <<< (_ + 1)
                      >>> (_ + 2)
                  )
                  (Just 1)
        x == Just 4
  log $ title <> "do notation: bind (_ + 1) and (_ + 2) to Just 1"
  log $ expect <> "(Just 4)\t"
    <> ( show
          $ ( do
                x <- Just 1
                y <- pure $ x + 1
                z <- pure $ y + 2
                pure z -- == Just 4
            )
          == Just 4
      )
  log $ title <> "do notation: bind (_ + 1) and (_ + 2) to Just 1"
  log $ expect <> "(Just 4)\t"
    <> ( show
          $ ( do
                x <- Just 1
                let
                  y = x + 1

                  z = y + 2
                pure z
            )
          == Just 4
      )
