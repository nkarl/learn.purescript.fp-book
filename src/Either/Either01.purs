module Either.Either01 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)

{-- DATA MODEL --}
data Either' a b -- sum type of binary choices with more freedom
  = Left a -- typically for error handling
  | Right b

derive instance genericEither' :: Generic (Either' a b) _

instance showEither' :: (Show a, Show b) => Show (Either' a b) where
  show = genericShow

derive instance functorEither' :: Functor (Either' a)

instance applyEither' :: Apply (Either' a) where
  apply (Left f) _ = Left f
  apply (Right f) x = f <$> x

instance applicativeEither' :: Applicative (Either' a) where
  pure = Right

instance bindEither' :: Bind (Either' a) where
  bind (Left x) _ = Left x
  bind (Right x) f = f x

instance monadEither' :: Monad (Either' a)

test :: Effect Unit
test = do
  log $ show $ (Right 1 :: Either' Unit _)
  log $ show $ (Right (_ + 1) `apply` Right 1 :: Either' Unit _)
  log $ show $ (Right (_ + 1) `apply` Right 1 :: Either' Unit _)
  log $ show $ (Right 1 :: Either' Unit _)
    >>= pure
    <<< (_ + 1)
    >>> (_ + 2)
  log
    $ show do
        x <- (Right 1 :: Either' Unit _)
        y <- pure $ x + 1
        pure y
  log
    $ show do
        x <- (Right 1 :: Either' Unit _)
        let
          y = x + 1
        pure y
