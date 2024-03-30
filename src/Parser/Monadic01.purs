module Parser.Monadic01 where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- | A State. a Tuple of String and a polymorphic type `a`.
type State a
  = Tuple String a

-- | An Action.
type Action e a
  = Failure e => String -> Either e (State a)

-- | Some State error. Produced by some unsuccessful action.
class Failure (e :: Type) where
  eof :: e

-- | A Context, which wraps an Action.
newtype Context e a
  = Context (Action e a)

data ErrorEOF
  = EOF

{-- DERIVE INSTANCES --}
derive instance genericErrorEOF :: Generic ErrorEOF _

{-- TYPECLASS INSTANCE SIGNATURES --}
instance showErrorEOF :: Show ErrorEOF where
  show = genericShow

instance parsingErrorEOF :: Failure ErrorEOF where
  eof = EOF

{-- helper --}
unwrap :: forall e a. Context e a -> Action e a
unwrap (Context f) = f

unwrap' :: forall a. Context ErrorEOF a -> Action ErrorEOF a
unwrap' = unwrap

{--
  DEFINE APPLICATIVE FUNCTOR INSTANCES
--}
-- f :: (a -> b)
-- cx :: Context e a
-- unwrap cx :: a
instance functorContext :: Functor (Context e) where
  --map :: forall a b. (a -> b) -> (Context e) a -> (Context e) b
  --map f c = Context \x -> (map f) `map` (unwrap c $ x)
  map f cx = Context \s -> (map f) `map` (unwrap cx $ s)

--f :: String -> Either err $ State (a -> b)
--g :: String -> Either err $ State  a
instance applyContext :: Apply (Context e) where
  --apply :: forall a b. (Context e) (a -> b) -> (Context e) a -> (Context e) b
  apply cf ca =
    Context \s -> do
      Tuple s'1 f <- unwrap cf s
      Tuple s'2 g <- unwrap ca s'1
      pure $ Tuple s'2 (f g)

instance applicativeContext :: Applicative (Context e) where
  --pure :: forall a. a -> (Context e) a
  pure c = Context \x -> pure (Tuple x c)

instance bindContext :: Bind (Context e) where
  bind c f =
    Context \x -> do
      Tuple s g <- unwrap c $ x
      unwrap (f g) $ s

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

test :: Effect Unit
test = do
  log $ show $ (unwrap' take1char_ $ "ABC")
  log $ show $ (unwrap' take2chars $ "ABC")
  log $ show $ (unwrap' take3chars $ "ABC")
  log $ show $ (unwrap' take3chars $ "AB")
  log
    $ show do
        let
          x = "ABC"

          y = (unwrap' take1char_) $ x
        (y)
  log $ show $ (unwrap' take3chars' $ "ABC")
  log $ show $ (unwrap' take3chars' $ "AB")
