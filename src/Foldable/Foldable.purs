module Foldable.Foldable where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)

class Foldable f where

  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldM :: forall a m. Monoid m => (a -> m) -> f a -> m

instance foldableList :: Foldable List where
  foldr _ acc Nil = acc
  foldr f acc (x : xs) = x `f` acc'
    where
    acc' = foldr f acc xs
  foldl _ acc Nil = acc
  foldl f acc (x : xs) = foldl f acc' xs
    where
    acc' = acc `f` x
  foldM _ Nil = mempty -- enum Empty
  foldM f ls = foldl g mempty ls
    where
    g acc = (<>) acc <<< f

data Result
  = Result
    { list :: List Int
    , acc :: Int
    , op :: String
    , fold :: String
    , result :: Maybe Int
    }

derive instance genericResult :: Generic Result _

instance showResult :: Show Result where
  show = genericShow

test :: Effect Unit
test = do
  let
    list = (10 : 20 : 30 : Nil)

    start = 1
  log $ show "Header: foldl"
  log $ show
    $ Result
        { list: list
        , op: "(+)"
        , fold: "L"
        , acc: start
        , result: Just $ foldl (+) start list
        }
  log $ show "Header: foldr"
  log $ show
    $ Result
        { list: list
        , op: "(+)"
        , fold: "R"
        , acc: start
        , result: Just $ foldr (+) start list
        }
