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

{--
  NOTE: high level view
    - An `Action` is a compressed `Process` which produces a `State`.
    - The State is a `Tuple` of `String` and some polymorphic type `a`.
--}
{-
  NOTE: input: a string such as "ABC"
-}
type State a
  = Tuple String a

{--
  NOTE:
    - The polymorphic type `e` (the left param of Either)
      - is _constrained_ to types that implement the typeclass Failure.
--}
type Process e a
  = (Failure e) => String -> Either e (State a)

{--
  NOTE: an action is a compressed process.
--}
newtype Action e a
  = Action (Process e a)

-- datatype for Error, a sum type that accounts for at least the EOF possibility.
data Error
  = EOF

derive instance genericErrorType :: Generic Error _

instance showErrorType :: Show Error where
  show = genericShow

{--
  NOTE:
    - Defining a typeclass `Failure` for some Type `e` (ie first-order Kind)
    - `e` is an error type whose class of actions includes at least an EOF action.
      - instance defined below
--}
class Failure (e :: Type) where
  eof :: e

instance failingError :: Failure  {- with -} Error where
  eof = EOF

{-
  NOTE: functor instances
-}
-- functor
instance functorAction :: Functor (Action e) where
  map f (Action mx) = Action \s -> h $ mx s
    where
    h = map g

    g = map f

-- apply
instance applyAction :: Apply (Action e) where
  apply (Action mf) (Action mx) =
    Action \s -> do
      Tuple s' f <- mf s
      Tuple s'' x <- mx s'
      pure $ Tuple s'' (f x)

-- applicative
instance applicativeAction :: Applicative (Action e) where
  pure x = Action \s -> pure $ Tuple s x

-- bind
instance bindAction :: Bind (Action e) where
  bind (Action mx) f =
    Action \s -> do
      Tuple s' x <- mx s
      Action g <- pure $ f x
      g s'

-- monad
instance monadAction :: Monad (Action e)

{-
  NOTE: data: Tuple of remaining string and a string of taken chars
-}
-- Action e Char :: String -> Failure e => Either e (Tuple String Char)
--class TakeOneChar e where
take1char :: forall e. Action e Char
--instance TakeOneChar Error where
take1char =
  Action \s -> case uncons s of
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
unwrap :: forall e a. Action e a -> Process e a
unwrap (Action f) = f

unwrap' :: forall a. Action Error a -> Process Error a
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
