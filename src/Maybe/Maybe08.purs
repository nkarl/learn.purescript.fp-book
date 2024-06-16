module Maybe.Maybe08 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)

{-- DATA MODEL --}
data Maybe' a
  = Nothing
  | Just a

derive instance eqMaybe' :: Eq a => Eq (Maybe' a)

derive instance ordMaybe' :: Ord a => Ord (Maybe' a)

derive instance genericMaybe' :: Generic (Maybe' a) _

instance showMaybe' :: Show a => Show (Maybe' a) where
  show = genericShow

{-- SIGNATURES --}
class Functor m where
  map :: forall a b. (a -> b) -> m a -> m b

class
  Functor m <= Apply m where
  apply :: forall a b. m (a -> b) -> m a -> m b

class
  Apply m <= Applicative m where
  unit :: forall a. a -> m a

class
  Applicative m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class
  Applicative m <= Join m where
  join :: forall a. m (m a) -> m a

class
  Bind m <= Monad m

{-- DEFINITIONS --}
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
  join = (flip $ bind) $ identity

instance monadMaybe' :: Monad Maybe'

infixl 1 bind as >>=

infixl 4 apply as <*>

flippedApply = flip apply

infixl 4 flippedApply as <.>

test :: Effect Unit
test = do
  let
    start = Just 1

    expected = Just 4

    compute = (_ + 2) <<< (_ + 1)

    unitCompute = unit <<< compute

    joining =
      join
        <<< map (unit <<< compute)

    joining' =
      join
        <<< map unit
        <<< map compute

    binding = \x ->
      x >>= unitCompute

    binding' = flip bind unitCompute
  -- APPLYING
  log $ show $ (Just 10)
    == ( (unit $ compute)
          <*> ( (unit $ compute) -- this is partial application, not composition
                <*> (binding' start)
            )
      )
  log $ show $ (Just 10)
    == ( apply (unit $ compute)
          $ apply (unit $ compute)
              (binding' start)
      )
  log $ show $ (Just 10)
    == ( binding' start
          <.> (unit $ compute) -- this is partial application, not compsition
          <.> (unit $ compute)
      )
  -- JOINING
  log $ show $ expected == joining start
  log $ show $ expected == joining' start
  log $ show $ (Just 10)
    == (join
        <<< map (unit <<< (compute <<< compute)) $ binding' start)
  -- BINDING
  log $ show $ expected == (binding start)
  -- BINDING MANY TIMES
  log $ show $ (Just 10)
    == ( binding' start
          >>= unitCompute
          >>= unitCompute
      )
  -- DOING
  log $ show $ (Just 10)
    == do
        x <- binding start
        unit $ compute <<< compute $ x

{--
  NOTE: joining and joining' show associative composition
--}
