module Maybe.Maybe04 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

data Maybe' a
  = Nothing
  | Just a

derive instance eqMaybe' :: Eq a => Eq (Maybe' a)

derive instance ordMaybe' :: Ord a => Ord (Maybe' a)

derive instance genericMaybe' :: Generic (Maybe' a) _

instance showMaybe' :: Show a => Show (Maybe' a) where
  show = genericShow

instance functorMaybe' :: Functor Maybe' where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe' :: Apply Maybe' where
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x

instance applicativeMaybe' :: Applicative Maybe' where
  pure = Just

instance bindMaybe' :: Bind Maybe' where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance monadMaybe' :: Monad Maybe'

test :: Effect Unit
test = do
  log $ show $ Just 1 == Just 1
  log $ show $ Just 2 == ((_ + 1) <$> Just 1)
  log $ show $ Just 2 == (Just (_ + 1) <*> Just 1)
  log $ show $ Just 2 == (Just (_ + 1) <*> pure 1)
  log $ show $ Just 4
    == ( Just 1
          >>= pure
          <<< (_ + 1)
          >>> (_ + 2)
      )
  log $ show $ Just 4
    == ( join
          $ ( pure
                <<< (_ + 1)
                >>> (_ + 2)
            )
          <$> (Just 1)
      )
  log $ show $ Just 4
    == ( do
          x <- Just 1
          y <- pure $ x + 1
          z <- pure $ y + 2
          pure z
      )
  log $ show $ Just 4
    == ( do
          x <- Just 1
          let
            y = x + 1

            z = y + 2
          pure z
      )
