module Monads.Stack.Transformer where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Control.Monad.State.Trans (StateT, get, put, runStateT)
import Control.Monad.Writer.Trans (class MonadTell, WriterT, runWriterT, tell)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Utils (print)

type AppStack e w s a
  = ExceptT e (WriterT w (StateT s Identity)) a

type AppM
  = AppStack String String Int Unit

log :: forall m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

type StackResult
  = Tuple (Tuple (Either String Unit) String) Int

type AppEffects
  = { log :: String
    , state :: Int
    , result :: Maybe Unit
    }

type AppResult
  = Tuple (Maybe String) AppEffects

results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) = Tuple (Just err) { log: l, state: s, result: Nothing }

results (Tuple (Tuple (Right _) l) s) = Tuple Nothing { log: l, state: s, result: Just unit }

runApp :: Int -> AppM -> StackResult
runApp s = unwrap <<< flip runStateT s <<< runWriterT <<< runExceptT

app :: AppM
app = do
  log "Starting App..."
  n <- get
  when (n == 0) $ void $ throwError "WE CANNOT HAVE A 0 STATE!"
  put $ n + 1
  log $ "Incremented State"
  pure unit

test :: Effect Unit
test = do
  print $ "Monads.Stack.Transformer tests"
  print $ runApp 0 app
  print $ runApp 99 app
