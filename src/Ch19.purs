module Ch19 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

-- Maybe
data Maybe a
  = Nothing
  | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  --map :: forall a b. (a -> b) -> (Maybe a -> Maybe b)
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  --apply :: forall a b. Maybe (a -> b) -> (Maybe a -> Maybe b)
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x -- map f x

instance applicativeMaybe :: Applicative Maybe where
  --pure :: a -> Maybe a
  pure = Just

instance bindMaybe :: Bind Maybe where
  --bind :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance monadMaybe :: Monad Maybe

testMaybe :: Effect Unit
testMaybe = do
  log $ show $ (_ * 10) <$> Just 20 -- `map` version: expects `Just 200`
  log $ show $ Just (_ * 10) <*> Just 20 -- `apply` version: expects `Just 200`
  log $ show $ Just (_ * 10) <*> pure 20 -- `pure` version: expects `Just 200`
  log $ show $ Just 20 >>= pure <<< (_ * 10) -- `bind` version: expects `Just 200`; `pure <<< (_ * 10)` is point-free
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

-- Either
data Either a b
  = Left a
  | Right b

derive instance functorEither :: Functor (Either a)

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

-- Apply
instance applyEither :: Apply (Either a) where
  apply (Left x) _ = Left x
  apply (Right f) x = f <$> x

-- Applicative
instance applicativeEither :: Applicative (Either a) where
  pure = Right

-- Bind
instance bindEither :: Bind (Either a) where
  --bind :: Either a -> (a -> Either b) -> Either b
  bind (Left x) _ = Left x
  bind (Right y) f = f y

-- Monad
instance monadEither :: Monad (Either a)

testEither :: Effect Unit
testEither = do
  log $ show $ (_ * 10) <$> (Right 20 :: Either Unit _)
  log $ show $ (Right (_ * 10) :: Either Unit _) <*> (Right 20)
  log $ show $ Right (_ * 10) <*> (pure 20 :: Either Unit _)
  log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
  log
    $ show do
        x <- Right 20 :: Either Unit _
        let
          y = x * 10
        pure y
  log $ show $ Right 20 >>= const (Left "error") >>= \y -> Right $ y + 42
  log
    $ show do
        _ <- Right 20
        y <- Left "error"
        pure $ y + 42
