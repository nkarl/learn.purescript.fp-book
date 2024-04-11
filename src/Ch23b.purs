module Ch23b where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.State.Trans (StateT, runStateT)
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, launchAff, makeAff, nonCanceler)
import Effect.Aff.Bus (Bus, BusRW)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

{--
  NOTE: DATA-TYPES and TYPE-ALIASES
--}
type StrBus = BusRW String -- `BusRW :: Bus ( read :: Cap, write :: Cap)` is a row type. It reads and writes strings.

type Config {-- product type --}
  = { bus :: StrBus } -- NOTE: could contain more properties, for example env variables.

type State {-- product type --}
  = { count :: Int }

--Callback  :: (Either Error a -> Effect Unit)
--makeAff   ::  Callback a -> Effect Canceler -> Aff a

{--
  NOTE:
    - each fiber is composed as a stack of Reader $ State $ Aff
    - a note on type definitions.
--}
--   FiberM a = ReaderT r       m                 a
type FiberM a = ReaderT Config (StateT State Aff) a

{--
  NOTE: CONSTRUCTS
--}

{--
  NOTE: how to run something inside a fiber?
    - we look at the definition of `FiberM`. "Running" means that given an input `FiberM` we produce some output wrapped in an `Aff`.
--}
runFiberM :: Config -> FiberM Unit -> Aff (Tuple Unit State)
runFiberM config = flip runStateT {count: 10}  -- flip so that we take the `StateT st` later
                    <<< flip runReaderT config  -- flip so that we take `ReaderT rt` later

randomAff :: Aff Number
randomAff =
  makeAff \cb -> do
    n <- random
    cb $ Right n
    pure nonCanceler

test :: Effect Unit
test = do
  log "placeholder"

{--
  NOTE: on `launchAff` and `forkAff`
    launchAff ::   Aff a -> Effect (Fiber a)
    - this is good for running an Aff in the main Effect, because it takes an `Aff` and (spawns and) returns a fiber inside an `Effect`. - however, what if we are in an `Aff`, and want to create another fiber?
        - we need `Aff a -> Aff    (Fiber a)`
    forkAff   ::   Aff a -> Aff    (Fiber a)
    forkAff lets us fork another fiber inside an Aff.
    launchAff launches a fiber inside an Effect. In other words, launchAff creates an patient-zero fiber.
--}

