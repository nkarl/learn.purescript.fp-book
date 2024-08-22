module Parser.Monadic02 where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Utils (print)

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

-- datatype for ErrorMsg, a sum type that accounts for at least the EOF possibility.
data ErrorMsg
  = EOF
  | InvalidChar String

derive instance genericErrorType :: Generic ErrorMsg _

instance showErrorType :: Show ErrorMsg where
  show = genericShow

{--
  NOTE:
    - Defining a typeclass `Failure` for some Type `e` (ie first-order Kind)
    - `e` is an error type whose class of actions includes at least an EOF action.
      - instance defined below
--}
class Failure (e :: Type) where
  eof :: e
  invalidChar :: String -> e

instance failingError :: Failure  {- with -} ErrorMsg where
  eof = EOF
  invalidChar = InvalidChar

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
--instance TakeOneChar ErrorMsg where
take1char =
  Action \s -> case uncons s of
    Nothing -> Left eof -- :: (Either ErrorMsg _)
    Just { head, tail } -> Right $ Tuple tail head

{-
  NOTE: take some chars from the string
-}
take2chars :: (Action ErrorMsg) String
take2chars = do
  c1 <- take1char
  c2 <- take1char
  pure $ fromCharArray [ c1, c2 ]

{-- helper --}
parse :: forall e a. Failure e => Action e a -> Process e a
parse (Action f) = f

fail' :: forall e a. (Failure e) => e -> Action e a
fail' e = Action $ \_ -> Left e

satisfy :: forall e. (Failure e) => String -> (Char -> Boolean) -> Action e Char
satisfy expected predicate =
  take1char
    >>= \c ->
        if predicate c then pure c else fail' $ invalidChar expected -- Left $ invalidChar expected

digit :: forall e. Failure e => Action e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: forall e. Failure e => Action e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

instance altParse :: Alt (Action e) where
  alt p1 p2 =
    Action
      $ \s -> case parse p1 s of
          Left _ -> parse p2 s
          Right x -> Right x

alphaNum :: forall e. Failure e => Action e Char
alphaNum = letter <|> digit

-- NOTE: unit test
test :: Effect Unit
test = do
  --print do
  --let
  --x = "ABC"
  --y = parse take1char $ x -- :: Either ErrorMsg _
  --y = unwrap' take1char $ x :: Either ErrorMsg _
  --y
  print $ (parse take2chars $ "ABC")
  print $ (parse take2chars $ "ABC")
  print $ (parse take2chars $ "AB")
