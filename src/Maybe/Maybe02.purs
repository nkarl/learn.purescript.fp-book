module Maybe.Maybe02 where

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

{--
  TYPE CLASSES SIGNATURES
--}
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class
  Functor p <= Apply p where
  apply :: forall a b. p (a -> b) -> p a -> p b

class
  Apply p <= Applicative p where
  pure :: forall a. a -> p a

class
  Applicative p <= Bind p where
  bind :: forall a b. p a -> (a -> p b) -> p b

class
  Applicative p <= Join p where
  join :: forall a. p (p a) -> p a

class (Applicative m, Join m) <= Monad m

{--
  TYPE CLASS INSTANCES
--}
instance functorMayb3 :: Functor Mayb3 where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMayb3 :: Apply Mayb3 where
  apply Nothing _ = Nothing
  apply (Just f) x = f `map` x

instance applicativeMayb3 :: Applicative Mayb3 where
  pure = Just

instance bindMayb3 :: Bind Mayb3 where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance joinMayb3 :: Join Mayb3 where
  join = (_ `bind` id)

--join Nothing = Nothing
--join (Just x) = x
instance monadMayb3 :: Monad Mayb3

{--
  HELPER FUNCTIONS
--}
id :: forall a. a -> a
id x = x

test :: Effect Unit
test = do
  let
    header = "show :: "

    endl = "\n\t"
  log $ header <> "(Just 2)" <> endl
    <> (show $ Just 2)
  log $ header <> "(Just 3) == (_ + 1) `map` (Just 2)" <> endl
    <> (show $ (_ + 1) `map` Just 2)
  log $ header <> "(Just 3) == Just (_ + 1) `apply` (Just 2)"
    <> (show $ Just (_ + 1) `apply` Just 2)
  log $ header <> "(Just 5) by JOIN-ing (_ + 1) and (_ + 2)" <> endl
    <> ( show
          $ join
          $ map
              ( pure
                  <<< (_ + 1)
                  >>> (_ + 2)
              )
              (Just 2)
      )
  log $ header <> "(Just 5) by BIND-ing (_ + 1) and (_ + 2)" <> endl
    <> ( show
          $ (Just 2)
              `bind`
                ( pure
                    <<< (_ + 1)
                    >>> (_ + 2)
                )
      )
  log $ header <> "(Just 5) using do-notation, arrow binding" <> endl
    <> ( show do
          x <- Just 2
          y <- pure $ x + 1
          z <- pure $ y + 2
          pure z
      )
  log $ header <> "(Just 5) using do-notation, let binding" <> endl
    <> ( show do
          x <- Just 2
          let
            y = x + 1

            z = y + 2
          pure z
      )
