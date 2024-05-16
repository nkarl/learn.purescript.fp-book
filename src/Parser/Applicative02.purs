module Parser.Applicative02 where

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
  = Failable err => String -> Either err (State a)

-- | Some State error. Produced by some unsuccessful action.
class Failable (err :: Type) where
  eof :: err

-- | A Context, which wraps an Action.
newtype Process err a
  = Process (Action err a)

{-- DERIVE INSTANCES --}
data Error
  = EOF

derive instance genericError :: Generic Error _

instance showErrorEOF :: Show (Error) where
  show = genericShow

--f     :: Tuple
--cx    :: String -> Either err (State a)
--cx s  ::           Either err (State a)
instance functorProcess :: Functor (Process e) where
  map f (Process px) =  --Process \s -> do --let --g = map f -- `map` instance of Data.Tuple partially takes `f` --x = cx s  -- x :: Either err (State a) --g <$> x -- `<$>` instance of Data.Either combining `g` and `x`
    Process $ h <<< px
    where
    h = map g

    g = map f

instance applyProcess :: Apply (Process e) where
  apply (Process pf) (Process px) =
    Process \s -> do
      Tuple s' f <- pf s
      Tuple s'' x <- px s'
      pure $ Tuple s'' $ f x

instance applicativeProcess :: Applicative (Process e) where
  pure x = Process \s -> Right $ Tuple s x

instance bindProcess :: Bind (Process e) where
  bind (Process px) f =
    Process \s -> do
      Tuple s' x <- px s
      Process g <- Right $ f x
      g s'

instance monadProcess :: Monad (Process e)

test :: Effect Unit
test = do
  log $ show $ "a"
