module Ch19 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Int.Bits ((.&.))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Utils (print)

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

oddTest :: Int -> Maybe Int
oddTest x = if x .&. 1 == 1 then Just x else Nothing

greaterThanTest :: Int -> Int -> Maybe Int
greaterThanTest min x = if x > min then Just x else Nothing

lessThanTest :: Int -> Int -> Maybe Int
lessThanTest max x = if x < max then Just x else Nothing

gaunlet :: Int -> Maybe Int
gaunlet =
  oddTest
    >=> greaterThanTest 10
    >=> lessThanTest 20

gaunlet2 :: Int -> Maybe Int
gaunlet2 =
  oddTest
    >=> (pure <<< (_ + 1))
    >=> greaterThanTest 10
    >=> lessThanTest 20

gaunlet3 :: Int -> Maybe Int
gaunlet3 x =
  pure x >>= oddTest
    >>= (pure <<< (_ + 1))
    >>= greaterThanTest 10
    >>= lessThanTest 20

testMaybe :: Effect Unit
testMaybe = do
  print $ gaunlet 14
  print $ gaunlet 1
  print $ gaunlet 93
  print $ gaunlet 17
  print $ gaunlet3 14
  print $ gaunlet3 1
  print $ gaunlet3 93
  print $ gaunlet3 17

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

oddTestE :: Int -> Either String Int
oddTestE x = if x .&. 1 == 1 then Right x else Left "Number is not odd"

greaterThanTestE :: Int -> Int -> Either String Int
greaterThanTestE min x =
  if x > min then
    Right x
  else
    Left $ "Number is not greater than " <> show min

lessThanTestE :: Int -> Int -> Either String Int
lessThanTestE max x =
  if x < max then
    Right x
  else
    Left $ "Number is not less than " <> show max

testEither :: Effect Unit
testEither = do
  print $ (_ * 10) <$> (Right 20 :: Either Unit _)
  print $ (Right (_ * 10) :: Either Unit _) <*> (Right 20)
  print $ Right (_ * 10) <*> (pure 20 :: Either Unit _)
  print $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
  print do
    x <- Right 20 :: Either Unit _
    let
      y = x * 10
    pure y
  print $ Right 20 >>= const (Left "error") >>= \y -> Right $ y + 42
  print do
    _ <- Right 20
    y <- Left "error"
    pure $ y + 42
