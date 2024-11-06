module Ch23b where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Trans as ReaderTrans
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Trans as StateTrans
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), delay, forkAff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

{--
  TYPES AND ALIASES
--}

-- | `BusRW` is a closed row type, that is a bus for reading and writing data (`String` in this case).
type StrBus = BusRW String

-- | could contain more properties, for example env variables.
type Reader = { bus :: StrBus }
-- | the program's state, contains the countDown.
type State = { countDown :: Int }

-- | This is our monad stack. This stack has only the State and Reader monads.
-- | each fiber is composed as a stack: Reader (State (Aff (Effect)))
-- | FiberM a ::ReaderT r      m                    a
type FiberM a = ReaderT Reader (StateT State (Aff)) a

{--
  ACTIONS
--}

type Callback a = Either Error a -> Effect Unit

--makeAff  :: forall a. Callback a -> Effect Canceler -> Aff a

affRandom' :: Aff Number
affRandom' = makeAff \cb -> do
  n <- random
  cb $ Right n -- no cases bc we interop on-system with a FFI `Math.random`. `Effect` wrapper is required for all FFI.
  pure nonCanceler

affRandom :: Aff Number
affRandom = liftEffect random -- replaces the callback with `liftEffect` and thus reduce LOC

{--
  - what does it mean to run something in a `FiberM`?
    - a `FiberM` is a monadic context. Inside, we perform some async actions (hence `Aff` at the base).
    - _running_ is an executive action; it doesn't need to produce any data. thus the generic Unit as output.
--}
-- | takes a BusRW and returns a fiber morphism for processing at a later time.
runFiberM :: BusRW String -> (FiberM Unit -> Aff Unit)
runFiberM bus =
  void
    <<< forkAff -- fork a new fiber for this call
    <<< flip runStateT { countDown: 10 } -- flip to take `s` first
    <<< flip runReaderT { bus } -- flip to take `r` first

-- | generates a random value with a delay; can be run async.
delayRandom :: Aff Number
delayRandom = delay (Milliseconds 1000.0) *> affRandom

-- | NOTE: subscribe fiber
logger :: FiberM Unit
logger = forever do
  { bus } <- ReaderTrans.ask
  s <- liftAff $ Bus.read bus
  log $ "Logger: " <> s

-- | NOTE: publish/broadcast fiber
randomGenerator :: String -> (Number -> Boolean) -> FiberM Unit
randomGenerator predLabel pred = do
  { countDown } <- StateTrans.get
  -- only run this action for 10s.
  unless (countDown <= 0) do
    { bus } <- ReaderTrans.ask
    liftAff do
      n <- delayRandom
      let
        output = "Found a value that is " <> predLabel <> " (" <> show n <> ")"
      when (pred n) $ Bus.write output bus
    --StateTrans.put { countDown: countDown - 1 }
    StateTrans.modify_ _ { countDown = countDown - 1 } -- counts down after every delayed generation (1.000 s)
    randomGenerator predLabel pred -- NOTE: recursive call only when `countDown <= 0`.

-- | tests the effect of module Ch23b
test :: Effect Unit
test =
  launchAff_ do
    bus <- Bus.make
    let
      run :: FiberM Unit -> Aff Unit
      run = runFiberM bus
    run $ logger
    run $ randomGenerator "greater than 0.5\t" (_ > 0.5)
    run $ randomGenerator "less    than 0.5\t" (_ < 0.5)
    run $ randomGenerator "greater than 0.1\t" (_ > 0.1)

{--
  NOTE: on `launchAff` and `forkAff`
    launchAff :: Aff a -> Effect (Fiber a)
    - this is good for running an Aff in the main Effect, because it takes an `Aff` and (spawns and) returns a fiber inside an `Effect`.
      - however, what if we are in an `Aff`, and want to create another fiber?
    - we need
    forkAff   :: Aff a -> Aff    (Fiber a)
    `forkAff` lets us create another fiber inside an Aff (unified context via `Aff`).
    `launchAff` creates an `Effect` context that contains an fiber.
--}

{--
  NOTE can it be replaced with `forE` instead?
    - `forE` would NOT work, because `countDown` is modified on each fiber spawning.
        - Leaving `countDown` in a global state allows for non-linear growth of fibers.
    - `forE`restricts fiber spawning to linear growth; only one fiber can be spawn at a time.
        - This is fine if there is only one predicate. However,
        - we need to spawn 3 fibers with indenpendent predicates concurrently.
    - in other words, `countDown` needs to "float" so that fibers can spawn; it cannot be an iterator a la `forE` in this case.
--}

