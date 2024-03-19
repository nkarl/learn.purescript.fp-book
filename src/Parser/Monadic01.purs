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
newtype Ctx e a
  = Ctx (Action e a)

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
unwrap :: forall e a. Ctx e a -> Action e a
unwrap (Ctx f) = f

unwrap' :: forall a. Ctx ErrorEOF a -> Action ErrorEOF a
unwrap' = unwrap

{--
  DEFINE APPLICATIVE FUNCTOR INSTANCES
--}
instance functorCtx :: Functor (Ctx e) where
  --map :: forall a b. (a -> b) -> (Ctx e) a -> (Ctx e) b
  map f c = Ctx \x -> (map f) `map` (unwrap c $ x)

--f :: String -> Either err $ State (a -> b)
--g :: String -> Either err $ State  a
instance applyCtx :: Apply (Ctx e) where
  --apply :: forall a b. (Ctx e) (a -> b) -> (Ctx e) a -> (Ctx e) b
  apply cf ca =
    Ctx \s -> do
      Tuple s'1 f <- unwrap cf s
      Tuple s'2 g <- unwrap ca s'1
      pure $ Tuple s'2 (f g)

instance applicativeCtx :: Applicative (Ctx e) where
  --pure :: forall a. a -> (Ctx e) a
  pure c = Ctx \x -> pure (Tuple x c)

instance bindCtx :: Bind (Ctx e) where
  bind c f =
    Ctx \x -> do
      Tuple s g <- unwrap c $ x
      unwrap (f g) $ s

-- | Uncons a single character from a String.
take1char_ :: forall e. (Ctx e) Char
take1char_ =
  Ctx \s -> case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head -- `head` is the polymorphic type a

-- | Destructures 2 characters from a String.
take2chars :: forall e. (Ctx e) (Tuple Char Char)
--take2chars = Tuple `map` take1char_ `apply` take1char_
take2chars = do
  c1 <- take1char_
  c2 <- take1char_
  pure $ Tuple c1 c2

-- | Destructures 3 characters from a String.
take3chars :: forall e. (Ctx e) (Tuple Char (Tuple Char Char))
take3chars = Tuple `map` take1char_ `apply` (Tuple `map` take1char_ `apply` take1char_)

take3chars' :: forall e. (Ctx e) String
take3chars' = do
  c1 <- take1char_
  c2 <- take1char_
  c3 <- take1char_
  pure $ fromCharArray [ c1, c2, c3 ]

test :: Effect Unit
test = do
  log $ show $ (unwrap' take1char_ $ "ABC" )
  log $ show $ (unwrap' take2chars $ "ABC" )
  log $ show $ (unwrap' take3chars $ "ABC" )
  log $ show $ (unwrap' take3chars $ "AB" )
  log
    $ show do
        let
          x = "ABC"
          y = (unwrap' take1char_) $ x
        (y )
  log $ show $ (unwrap' take3chars' $ "ABC" )
  log $ show $ (unwrap' take3chars' $ "AB" )
