module Ch19 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Maybe a
  = Nothing
  | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  --map :: (a -> b) -> Maybe a -> Maybe b
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  --apply :: Maybe (a -> b) -> Maybe a -> Maybe b
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x

instance applicativeMaybe :: Applicative Maybe where
  --pure :: a -> Maybe a
  pure = Just

instance bindMaybe :: Bind Maybe where
  --bind :: Maybe a -> (a -> Maybe b) -> Maybe b
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance monadMaybe :: Monad Maybe

test :: Effect Unit
test = do
  log $ show $ (_ * 10) <$> Just 20 -- `map`   version: expects `Just 200`
  log $ show $ Just (_ * 10) <*> Just 20 -- `apply` version: expects `Just 200`
  log $ show $ Just (_ * 10) <*> pure 20 -- `pure`  version: expects `Just 200`
  log $ show $ Just 20 >>= pure <<< (_ * 10) -- `bind`  version: expects `Just 200`; `pure <<< (_ * 10)` is point-free
  log
    $ show do -- `do` monadic version: expects `Just 200`
        x <- Just 20
        let
          y = x * 10
        pure y
  log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42 -- `bind` version: expects `Nothing`
  log
    $ show do -- `do` monadic version: expects `Nothing`
        _ <- Just 20
        y <- Nothing
        pure $ y + 42
