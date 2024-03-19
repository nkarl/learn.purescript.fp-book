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

instance functorCtx :: Functor (Ctx e) where
  map f (Ctx c) = Ctx \x -> map (map f) (c x)

derive instance genericErrorEOF :: Generic ErrorEOF _

instance showErrorEOF :: Show (ErrorEOF) where
  show = genericShow

test :: Effect Unit
test = do
  log $ show $ "a"
