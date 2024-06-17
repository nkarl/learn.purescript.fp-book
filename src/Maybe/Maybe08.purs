module Maybe.Maybe08 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

{-- DATA MODEL --}
data Maybe' a
  = Nothing
  | Just a

derive instance eqMaybe'      :: Eq  a => Eq  (Maybe' a)
derive instance ordMaybe'     :: Ord a => Ord (Maybe' a)
derive instance genericMaybe' :: Generic (Maybe' a) _

instance showMaybe' :: Show a => Show (Maybe' a) where
  show = genericShow

{-- SIGNATURES --}
class Functor m where
  map :: forall a b. (a -> b) -> m a -> m b

class Functor m <= Apply m where
  apply :: forall a b. m (a -> b) -> m a -> m b

class Apply m <= Applicative m where
  unit :: forall a. a -> m a

class Applicative m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class Applicative m <= Join m where
  join :: forall a. m (m a) -> m a

class (Join m, Bind m) <= Monad m

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
  join = (flip bind) identity

instance monadMaybe' :: Monad Maybe'

{-- ADDITIONAL SIGNATURES AND ALIASES --}
flippedApply :: forall a b m. Apply m => m a -> m (a -> b) -> m b
flippedApply = flip apply

print :: forall a m. MonadEffect m => Show a => a -> m Unit
print = log <<< show

infixl 1 bind         as >>=
infixl 4 apply        as <*>
infixl 4 flippedApply as <.>

{-- TEST --}
test :: Effect Unit
test = do
  let
    x           = Just 1
    expected    = Just 7
    compute     = (_ + 2) <<< (_ + 1)
    unitApply   = unit $    compute -- this is partial application of a function
    unitCompose = unit <<<  compute -- this is function composition
    joining     = join
                    <<< map (unit <<< compute)
    joining'    = join
                    <<< map unit
                    <<< map compute

  -- JOINING
  log $ show $ (Just 4) == joining  x
  log $ show $ (Just 4) == joining' x

  -- APPLYING MANY TIMES
  print $ expected == ( unitApply
                              <*> ( unitApply
                                    <*> x ) )
  print $ expected == ( apply unitApply
                              $ apply unitApply x )
  print $ expected == ( x
                              <.> unitApply
                              <.> unitApply )   -- NOTE: very similar to BINDING
  -- JOINING MANY TIMES
  print $ expected == ( join
                              <<< map (unit <<< (compute <<< compute))
                              $ x )
  -- BINDING MANY TIMES
  print $ expected == ( x
                              >>= unitCompose
                              >>= unitCompose ) -- NOTE: similar to flippedApply
  -- COMPUTING MANY TIMES INSIDE DO
  print $ expected == do
                            a <- x
                            let
                              b = compute a
                              c = compute b
                            unit c

{--
  NOTE: joining and joining' show associative composition
--}
