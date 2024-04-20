module Ch23b where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Trans (StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, launchAff_)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

{--
  NOTE: TYPES AND ALIASES
--}
-- `BusRW :: Bus ( read :: Cap, write :: Cap)` is a row type.
-- | is a Bus that reads and writes strings.
type StrBus = BusRW String
-- | could contain more properties, for example env variables.
type Reader = { bus :: StrBus } 
-- the program's state, contains the count.
type State  = { count :: Int }
{--
  NOTE: can it be replaced with `forE` instead?
    - `forE` would NOT work, because `count` is modified on each fiber spawning.
        - Leaving `count` in a global state allows for non-linear growth of fibers.
    - `forE`restricts fiber spawning to linear growth; only one fiber can be spawn at a time.
        - This is fine if there is only one predicate. However,
        - we need to spawn 3 fibers with indenpendent predicates concurrently.
    - in other words, `count` needs to "float" so that fibers can spawn; it cannot be a linear iterator a la `forE` in this case.
--}

-- each fiber is composed as a stack of Reader $ State $ Aff
--  FiberMS a :: ReaderT r      m                    a
type FiberMS a = ReaderT Reader (StateT State (Aff)) a

{--
  NOTE: ACTIONS
--}

--Callback :: (Either Error a -> Effect Unit)
--makeAff  :: Callback a -> Effect Canceler -> Aff a
-- | a random number lifted into Aff.
affRandom :: Aff Number
affRandom =
  liftEffect random -- we replace callbacks with `liftEffect` and thus reduce LOC
  --makeAff \cb -> do
    --n <- random
    --cb $ Right n
    --pure nonCanceler

{--
  - how to run something inside a fiber?
    - "running" means that given an input `FiberMS`, we produce some output wrapped in an `Aff`.
    - we look at the definition of `FiberMS`, ie our monad stack.
      - ReaderT is outermost, then StateT and finally Aff a.
      - we don't need anything return from Aff, so we return a `Unit` wrapped by Aff.
--}
-- | takes a BusRW and returns a fibric function that will process later.
runFiberM :: BusRW String -> (FiberMS Unit -> Aff Unit) -- (Tuple Unit State)
runFiberM bus =
  void -- coerce the polymorphic type `a` to Unit
    <<< forkAff -- fork a new fiber for this call
    -- runStateT  :: (StateT  s m a) -> s -> m a
    <<< flip runStateT  { count: 10 } -- flip to take `s` first and `StateT s m a` later, partial
    -- runReaderT :: (ReaderT r m a) -> r -> m a
    <<< flip runReaderT { bus }       -- flip to take `r` first and `ReaderT r m a` later, partial

-- | subscribe fiber
logger :: FiberMS Unit
logger = forever do
  { bus } <- ask
  s       <- liftAfftoFiberM $ Bus.read bus
  log $ "Logger: " <> s

-- | factored out
liftAfftoFiberM :: Aff ~> FiberMS
liftAfftoFiberM = lift <<< lift

-- | the desired randomized value wrapped in Aff.
delayRandom :: Aff Number
delayRandom = delay (Milliseconds 1000.0) *> affRandom

-- | publish/broadcast fiber
randomGenerator :: String -> (Number -> Boolean) -> FiberMS Unit
randomGenerator predLabel pred = do
  { count } <- get
  unless (count <= 0) do
     { bus } <- ask
     liftAfftoFiberM do
        n <- delayRandom
        let output = "Found a value that is " <> predLabel <> " (" <> show n <> ")"
        when (pred n) $ Bus.write output bus
     --put { count: count - 1 }
     modify_ _ {count = count - 1}
     randomGenerator predLabel pred

-- | tests the effect of module Ch23b
test :: Effect Unit
test = launchAff_ do
  bus <- Bus.make
  let forkFiberM = runFiberM bus
  forkFiberM $ logger
  forkFiberM $ randomGenerator "greater than 0.5\t" (_ > 0.5)
  forkFiberM $ randomGenerator "less    than 0.5\t" (_ < 0.5)
  forkFiberM $ randomGenerator "greater than 0.1\t" (_ > 0.1)

{--
  NOTE: on `launchAff` and `forkAff`
    launchAff ::   Aff a -> Effect (Fiber a)
    - this is good for running an Aff in the main Effect, because it takes an `Aff` and (spawns and) returns a fiber inside an `Effect`. - however, what if we are in an `Aff`, and want to create another fiber?
        - we need `Aff a -> Aff    (Fiber a)`
    forkAff   ::   Aff a -> Aff    (Fiber a)
    forkAff lets us fork another fiber inside an Aff.
    launchAff launches a fiber inside an Effect. In other words, launchAff creates an patient-zero fiber.
--}
