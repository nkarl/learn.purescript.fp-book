module Parser.Monadic02 where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

{-
  NOTE: input: a string such as "ABC"
-}
type {-- alias --} State a = Tuple String a

-- The polymorphic type `e` (the first param of Either) is constrained to those that have an instance of typeclass Failing.
type {-- alias --} Action e a = String -> Failing e => Either e (State a)

newtype Process e a = Process (Action e a)

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
instance Functor (Process e) where
  map f (Process px) = Process \s -> map f <$> px s

-- apply
instance Apply (Process e) where
  apply (Process pf) (Process px) =
    Process \s -> do
      Tuple s' f <- pf s
      map f <$> px s'

-- applicative
instance Applicative (Process e) where
  pure x = Process \s -> pure $ Tuple s x

-- bind
instance Bind (Process e) where
  bind (Process px) f =
    Process \s -> do
      Tuple s' x <- px s
      f x # \(Process f') -> f' s'

-- monad
instance Monad (Process e)

{-
  NOTE: data: Tuple of remaining string and a string of taken chars
-}
-- Process e Char :: String -> Failing e => Either e (Tuple String Char)
class TakeOneChar e where
  take1char :: forall e. Process e Char

instance TakeOneChar Error where
  take1char =
    Process \s -> do
      let
          x = eof
          --y = EOF
      case uncons s of
        Nothing -> Left x-- y :: (Either Error _)
        Just { head, tail } -> Right $ Tuple tail head

{-
  NOTE: take some chars from the string
-}
--take2chars :: (Process Error) String
--take2chars = do
  --c1 <- take1char
  --c2 <- take1char
  --pure $ fromCharArray [ c1, c2 ]

{-- helper --}
unwrap :: forall e a. (Process e) a -> Action e a
unwrap (Process f) = f

unwrap' :: forall a. (Process Error) a -> (Action Error) a
unwrap' = unwrap

-- NOTE: unit test
test :: Effect Unit
test = do
  log
    $ show do
        let
          x = "ABC"
          y = take1char $ x -- :: Either Error _
        y
  --log $ show $ (unwrap' take2chars $ "ABC")
  --log $ show $ (unwrap' take2chars $ "ABC")
  --log $ show $ (unwrap' take2chars $ "AB")
