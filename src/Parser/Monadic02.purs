module Parser.Monadic02 where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (uncons)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

{-
  NOTE: input: a string such as "ABC"
-}
type {-- alias --} State a = Tuple String a

-- The polymorphic type `e` (the first param of Either) is constrained to those that have an instance of typeclass Failing.
type {-- alias --} Process e a = String -> Failing e => Either e (State a)

newtype Action e a = Action (Process e a)

-- Error
data Error = EOF

derive instance Generic Error _

instance Show Error where
  show = genericShow

-- the polymorphic type `e` is a Type and not a Kind
class Failing (e :: Type) where
  eof :: e

instance Failing Error where
  eof = EOF

{-
  NOTE: functor instances
-}
-- functor
instance Functor (Action e) where
  map f (Action px) = Action \s -> map f <$> px s

-- apply
instance Apply (Action e) where
  apply (Action pf) (Action px) =
    Action \s -> do
      Tuple s' f <- pf s
      map f <$> px s'

-- applicative
instance Applicative (Action e) where
  pure x = Action \s -> pure $ Tuple s x

-- bind
instance Bind (Action e) where
  bind (Action px) f =
    Action \s -> do
      Tuple s' x <- px s
      --f x # \(Action f') -> f' s'
      let exec = \(Action f') -> f' s'
      exec $ f x

-- monad
instance Monad (Action e)

{-
  NOTE: data: Tuple of remaining string and a string of taken chars
-}
-- Action e Char :: String -> Failing e => Either e (Tuple String Char)
--class TakeOneChar e where
take1char :: forall e. Action e Char
--instance TakeOneChar Error where
take1char =
    Action \s ->
      case uncons s of
        Nothing -> Left eof -- :: (Either Error _)
        Just { head, tail } -> Right $ Tuple tail head

{-
  NOTE: take some chars from the string
-}
--take2chars :: (Action Error) String
--take2chars = do
  --c1 <- take1char
  --c2 <- take1char
  --pure $ fromCharArray [ c1, c2 ]

{-- helper --}
unwrap :: forall e a. (Action e) a -> Process e a
unwrap (Action f) = f

unwrap' :: forall a. (Action Error) a -> (Process Error) a
unwrap' = unwrap

-- NOTE: unit test
test :: Effect Unit
test = do
  log
    $ show do
        let
          x = "ABC"
          y = unwrap' take1char $ x -- :: Either Error _
        y
  --log $ show $ (unwrap' take2chars $ "ABC")
  --log $ show $ (unwrap' take2chars $ "ABC")
  --log $ show $ (unwrap' take2chars $ "AB")
