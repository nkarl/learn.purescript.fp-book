module Parser.Applicative01 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (uncons)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | A State. a Tuple of String and a polymorphic type `a`.
type State a
  = Tuple String a

-- | An Action.
type Action err a
  = Failure err => (String -> Either err (State a))

-- | Some State error. Produced by some unsuccessful action.
class Failure (err :: Type) where
  eof :: err

-- | A Context, which wraps an Action.
newtype Ctx err a
  = Ctx (Action err a)

{-- DERIVE INSTANCES --}
data ErrorEOF
  = EOF

derive instance genericErrorEOF :: Generic ErrorEOF _

instance showErrorEOF :: Show (ErrorEOF) where
  show = genericShow

{-- TYPECLASS INSTANCE SIGNATURES --}
instance parsingErrorEOF :: Failure ErrorEOF where
  eof = EOF

{-- helper --}
unwrap :: forall err a. (Ctx err) a -> (Action err) a
unwrap (Ctx f) = f

{--
  DEFINE APPLICATIVE FUNCTOR INSTANCES
--}

instance functorCtx :: Functor (Ctx e) where
  --map :: forall a b. (a -> b) -> (Ctx e) a -> (Ctx e) b
  map f (Ctx cx) =
    Ctx \s ->
      (map f) {- `map` instance of Data.Either, which partialy takes `f` -}
        <$> (cx s) {- `<$>` instance of Data.Functor -}
  --map f (Ctx cx) = Ctx \s -> do
     --Tuple s' x <- cx s
     --pure $ Tuple s' $ f x

--cf      :: String -> Either error {- or -} State (a -> b)
--cf s    ::           Either error          State (a -> b)
--cx      :: String -> Either error {- or -} State  a
--cx s'1  ::           Either error          State  a
instance applyCtx :: Apply (Ctx e) where
  --apply :: forall a b. (Ctx e) (a -> b) -> (Ctx e) a -> (Ctx e) b
  apply (Ctx c'f) (Ctx c'x) =
    Ctx \s -> case c'f s of
      Left error -> Left error
      Right (Tuple s'1 f) -> case c'x s'1 of
        Left error -> Left error
        Right (Tuple s'2 x) -> Right (Tuple s'2 (f x))

instance applicativeCtx :: Applicative (Ctx e) where
  --pure :: forall a. a -> (Ctx e) a
  pure x = Ctx \s -> Right (Tuple s x)

-- | Uncons a single character from a String.
take1char :: forall e. (Ctx e) Char
take1char =
  Ctx \s -> case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head -- `head` is the polymorphic type a

-- | Destructures 2 characters from a String.
take2chars :: forall e. (Ctx e) (Tuple Char Char)
take2chars = Tuple `map` take1char `apply` take1char

-- | Destructures 3 characters from a String.
take3chars :: forall e. (Ctx e) (Tuple Char (Tuple Char Char))
take3chars = Tuple `map` take1char `apply` (Tuple `map` take1char `apply` take1char)

unwrap' :: forall a. (Ctx ErrorEOF) a -> (Action ErrorEOF) a
unwrap' = unwrap

test :: Effect Unit
test = do
  log $ show $ (unwrap' take1char $ "ABC")
  log $ show $ (unwrap' take2chars $ "ABC")
  log $ show $ (unwrap' take3chars $ "ABC")
  log $ show $ (unwrap' take3chars $ "AB")
  log
    $ show do
        let
          x = "ABC"
          y = unwrap' take1char $ x
        y
