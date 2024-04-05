module Ch23b where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.State.Trans (StateT, runStateT)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Bus (BusRW)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

type Config
  = { bus :: BusRW String }

type State
  = { count :: Int }

type FiberM a
  = ReaderT Config (StateT State Aff) a

runFiberM :: BusRW String -> FiberM Unit -> Aff (Tuple Unit State)
runFiberM bus =
  flip runStateT { count: 10 }
    <<< flip runReaderT { bus }

randomAff :: Aff Number
randomAff = liftEffect random

test :: Effect Unit
test = do
  log "placeholder"
