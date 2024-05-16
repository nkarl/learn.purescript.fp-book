module Foldable.Foldable where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)

class Foldable f where
  -- -- foldr (+) acc [1, 2, 3] = 1 + (2 + (3 + acc))
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  -- -- foldl (+) acc [1, 2, 3] = ((acc + 1) + 2) + 3
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  -- -- map a function `f` onto a Foldable `ls` producing a Monoid `m`
  foldM :: forall a m. Monoid m => (a -> m) -> f a -> m

instance foldableList :: Foldable List where
  foldr _ acc Nil = acc
  foldr f acc (x : xs) = x `f` (foldr f acc xs)
  foldl _ acc Nil = acc
  foldl f acc (x : xs) = foldl f acc' xs
    where
    acc' = acc `f` x
  foldM _ Nil = mempty -- enum Empty
  foldM f ls = foldl (\acc x -> acc <> f x) mempty ls

data Result
  = Result
    { list :: List Int
    , acc :: Int
    , op :: String
    , fold :: String
    , result :: Int
    }

derive instance genericResult :: Generic Result _

instance showResult :: Show Result where
  show = genericShow

test :: Effect Unit
test = do
  let
    ls = (10 : 20 : 30 : Nil)

    acc = 1
  log $ show $ Result { list: ls, acc: acc, op: "(+)", fold: "L", result: foldl (+) acc ls }
  log $ show $ Result { list: ls, acc: acc, op: "(+)", fold: "R", result: foldr (+) acc ls }
