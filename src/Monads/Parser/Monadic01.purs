module Parser.Monadic01 where

import Prelude
import Control.Plus (class Alt)
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

-- | A State. a Tuple of String and a polymorphic type `a`.
type State a
  = Tuple String a

-- | An Action.
type Action e a
  = Failure e => (String -> Either e (State a))

--newtype Action e a = Action (Failure e => String -> Either e (State a))
-- | Some State error. Produced by some unsuccessful action.
class Failure (e :: Type) where
  eof :: e
  invalidChar :: String -> e

-- | A Context, which wraps an Action.
newtype Context e a
  = Context (Action e a)

data ErrorEOF
  = EOF
  | InvalidChar String

{-- DERIVE INSTANCES --}
derive instance genericErrorEOF :: Generic ErrorEOF _

{-- TYPECLASS INSTANCE SIGNATURES --}
instance showErrorEOF :: Show ErrorEOF where
  show = genericShow

instance parsingErrorEOF :: Failure ErrorEOF where
  eof = EOF
  invalidChar = InvalidChar

{-- helper --}
unwrap :: forall e a. (Context e) a -> (Action e) a
unwrap (Context f) = f

{--
  DEFINE APPLICATIVE FUNCTOR INSTANCES
--}
-- f          :: (a -> b)
-- cx         :: String -> (Either e (State a))
-- cx s       ::            Either e (State a)
-- map f      ::                   (Context e) a -> (Context e) b
instance functorContext :: Functor (Context e) where
  --map :: forall a b. (a -> b) -> (Context e) a -> (Context e) b
  --map f (Context cx) = Context \s -> (map f) `map` (cx s)
  map f (Context cx) =
    Context \s -> do
      Tuple s' x <- cx s
      pure $ Tuple s' $ f x

--c'f     :: String -> (Either err State (a -> b))
--c'f s   ::            Either err State (a -> b)
--c'x     :: String -> (Either err State  a)
--c'x s'1 ::            Either err State  a
instance applyContext :: Apply (Context e) where
  --apply :: forall a b. (Context e) (a -> b) -> (Context e) a -> (Context e) b
  --apply cf cx
  apply (Context c'f) (Context c'x) =
    Context \s -> do
      Tuple s'1 f <- c'f s
      Tuple s'2 x <- c'x s'1
      pure $ Tuple s'2 (f x)

instance applicativeContext :: Applicative (Context e) where
  --pure :: forall a. a -> (Context e) a
  pure x = Context \s -> Right (Tuple s x)

instance bindContext :: Bind (Context e) where
  -- bind :: forall a b. (Context e) a -> (a -> Context e a) -> Context e b
  bind (Context cx) f =
    Context \s -> do
      Tuple s' x <- cx s
      --unwrap (f x) $ s'
      f x # \(Context g) -> g s'

-- | Uncons a single character from a String.
take1char_ :: forall e. (Context e) Char
take1char_ =
  Context \s -> case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head -- `head` is the polymorphic type a

-- | Destructures 2 characters from a String.
take2chars :: forall e. (Context e) (Tuple Char Char)
--take2chars = Tuple `map` take1char_ `apply` take1char_
take2chars = do
  c1 <- take1char_
  c2 <- take1char_
  pure $ Tuple c1 c2

-- | Destructures 3 characters from a String.
take3chars :: forall e. (Context e) (Tuple Char (Tuple Char Char))
take3chars = Tuple `map` take1char_ `apply` (Tuple `map` take1char_ `apply` take1char_)

take3chars' :: forall e. (Context e) String
take3chars' = do
  c1 <- take1char_
  c2 <- take1char_
  c3 <- take1char_
  pure $ fromCharArray [ c1, c2, c3 ]

fail' :: forall e a. (Failure e) => e -> Context e a
fail' e = Context $ \_ -> Left e

satisfy :: forall e. (Failure e) => String -> (Char -> Boolean) -> Context e Char
satisfy expected predicate =
  take1char_
    >>= \c ->
        if predicate c then pure c else fail' $ invalidChar expected -- Left $ invalidChar expected

digit :: forall e. Failure e => Context e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: forall e. Failure e => Context e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

instance altParse :: Alt (Context e) where
  alt p1 p2 =
    Context
      $ \s -> case unwrap p1 s of
          Left _ -> unwrap p2 s
          Right x -> Right x

test :: Effect Unit
test = do
  print $ ((unwrap take1char_ $ "ABC") :: Either ErrorEOF _)
  print $ ((unwrap take2chars $ "ABC") :: Either ErrorEOF _)
  print $ ((unwrap take3chars $ "ABC") :: Either ErrorEOF _)
  print $ ((unwrap take3chars $ "AB") :: Either ErrorEOF _)
  print $ ((unwrap take3chars' $ "ABC") :: Either ErrorEOF _)
  print $ ((unwrap take3chars' $ "AB") :: Either ErrorEOF _)
