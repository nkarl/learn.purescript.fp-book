module Maybe.Maybe08 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Prelude 
import Utils (print)

{-- DATA MODEL --}

-- The Maybe type
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
  map :: forall a b. (a -> b) -> Maybe' a -> Maybe' b
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

flippedBind :: forall m a b. Bind m => (a -> m b) -> m a -> m b
flippedBind  = flip bind

infixl 1 bind         as >>=
infixl 1 flippedBind  as =<<
infixl 4 apply        as <*>
infixl 4 flippedApply as <.>

{-- TEST --}
test :: Effect Unit
test = do
  let
    x           = Just 1 :: Maybe' Int
    expected    = Just 7 :: Maybe' Int
    compute     = (_ + 2) <<< (_ + 1)

    unitComputeCurried  = unit compute      -- this is partial application of a function
    unitComputeComposed = unit <<<  compute -- this is function composition

    -- NOTE: joining and joining' demonstrate the law of associative composition
    joining     = join <<< map (unit <<< compute)
    joining'    = join <<< map unit  <<< map compute

  -- JOINING
  print $ (Just 4) == joining  x
  print $ (Just 4) == joining' x

  -- APPLYING MANY TIMES
  print $ expected == ( apply unitComputeCurried
                          ( apply unitComputeCurried x ) )
  print $ expected == ( unitComputeCurried
                          <*> ( unitComputeCurried
                                  <*> x ) )
  print $ expected == ( x
                          <.> unitComputeCurried
                          <.> unitComputeCurried )  -- NOTE: very similar to BINDING
  -- JOINING MANY TIMES
  print $ expected == ( join <<< map (unit <<< compute <<< compute) ) x
  -- BINDING MANY TIMES
  print $ expected == ( (unit <<< compute <<< compute) =<< x )
  print $ expected == ( x >>= (unit <<< compute <<< compute) )
  print $ expected == ( x
                          >>= unitComputeComposed
                          >>= unitComputeComposed ) -- NOTE: similar to flippedApply
  -- COMPUTING MANY TIMES INSIDE DO
  print $ expected == do
                        a <- x
                        let
                          b = compute a
                          c = compute b
                        unit c
  print $ Just 5 == unit (compute 2)
