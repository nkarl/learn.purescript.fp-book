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

-- NOTE: input: a string such as "ABC"
type State a
  = Tuple String a

type Action' e a
  = Failure e => String -> Either e (State a)

newtype Action e a
  = Action (Action' e a)

class Failure (e :: Type) where
  eof :: e

-- eof Error
data ErrEOF
  = EOF

derive instance genericErrEOF :: Generic ErrEOF _

instance showErrEOF :: Show ErrEOF where
  show = genericShow

instance parseErrEOF :: Failure ErrEOF where
  eof = EOF

-- NOTE: functor instances
-- functor
instance functorAction :: Functor (Action e) where
  map f (Action ax) = Action \s -> map f <$> ax s

-- apply
instance applyAction :: Apply (Action e) where
  apply (Action af) (Action ax) =
    Action \s -> do
      Tuple s' f <- af s
      map f <$> ax s'

-- applicative
instance applicativeAction :: Applicative (Action e) where
  pure x = Action \s -> pure $ Tuple s x

-- bind
instance bindAction :: Bind (Action e) where
  bind (Action ax) f =
    Action \s -> do
      Tuple s' x <- ax s
      f x # \(Action f') -> f' s'

-- monad
instance monadAction :: Monad (Action e)

-- NOTE: data: Tuple of remaining string and a string of taken chars
take1char' :: forall e. (Action e) Char
take1char' =
  Action \s -> case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head

-- NOTE: take some chars from the string
take2chars :: forall e. (Action e) String
take2chars = do
  c1 <- take1char'
  c2 <- take1char'
  pure $ fromCharArray [ c1, c2 ]

{-- helper --}
unwrap :: forall e a. (Action e) a -> (Action' e) a
unwrap (Action f) = f

unwrap' :: forall a. (Action ErrEOF) a -> (Action' ErrEOF) a
unwrap' = unwrap

-- NOTE: unit test
test :: Effect Unit
test = do
  log
    $ show do
        let
          x = "ABC"
          y = (unwrap' take1char') $ x
        (y)
  log $ show $ (unwrap' take2chars $ "ABC")
  log $ show $ (unwrap' take2chars $ "AB")
  log $ show $ (unwrap' take2chars $ "A")
