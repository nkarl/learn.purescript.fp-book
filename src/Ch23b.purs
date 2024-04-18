module Ch23b where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Trans (StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Effect (Effect, forE)
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
-- the program's state, contains the count. NOTE: can it be replaced with forE instead?
type State  = { count :: Int }

{--
  NOTE:
    - each fiber is composed as a stack of Reader $ State $ Aff
     FiberM a :: ReaderT r      m                 a
--}
type FiberM a = ReaderT Reader (StateT State Aff) a

{--
  NOTE: CONSTRUCTS
--}

{--
  - Instead of using callbacks, we use `liftEffect` and thus reduce LOC
--}
--Cb      :: (Either Error a -> Effect Unit)
--makeAff ::  Cb a -> Effect Canceler -> Aff a
-- | a random number lifted into Aff.
affRandom :: Aff Number
affRandom =
  liftEffect random
  --makeAff \cb -> do
    --n <- random
    --cb $ Right n
    --pure nonCanceler

{--
  - how to run something inside a fiber?
    - we look at the definition of `FiberM`.
  - given an input `FiberM`, we produce some output wrapped in an `Aff`.
--}
runFiberM :: BusRW String -> FiberM Unit -> Aff Unit -- (Tuple Unit State)
runFiberM bus = 
  void <<< forkAff
    -- runReaderT :: ReaderT r m a -> r -> m a
    <<< flip runReaderT { bus }       -- flip to take `r` first and `ReaderT r m a` later
    -- runStateT  :: StateT  s m a -> s -> m a
    >>> flip runStateT  { count: 10 } -- flip to take `s` first and `StateT s m a` later

-- | subscribe fiber
logger :: FiberM Unit
logger = forever do
  { bus } <- ask
  s       <- liftAfftoFiberM $ Bus.read bus
  log $ "Logger: " <> s

-- | factored out
liftAfftoFiberM :: Aff ~> FiberM
liftAfftoFiberM = lift <<< lift

-- | the desired randomized value wrapped in Aff.
delayRandom :: Aff Number
delayRandom = delay (Milliseconds 1000.0) *> affRandom

-- | publish/broadcast fiber
randomGenerator :: String -> (Number -> Boolean) -> FiberM Unit
randomGenerator predLabel pred = do
  { count } <- get
  unless (count <= 0) do
     { bus } <- ask
     liftAfftoFiberM do
        n <- delayRandom
        let
            output = "Found a value that is " <> predLabel <> " (" <> show n <> ")"
        when (pred n) $ flip Bus.write bus $ output
     --put { count: count - 1 }
     modify_ _ {count = count - 1}
     randomGenerator predLabel pred

-- | tests the effect of module Ch23b
test :: Effect Unit
test = launchAff_ do
  bus <- Bus.make
  let forkFiberM = runFiberM bus
  forkFiberM $ logger
  forkFiberM $ randomGenerator "greater than 0.5" (_ > 0.5)
  forkFiberM $ randomGenerator "less than 0.5"    (_ < 0.5)
  forkFiberM $ randomGenerator "greater than 0.1" (_ > 0.1)
  -- randomGenerator
  pure unit

{--
  NOTE: on `launchAff` and `forkAff`
    launchAff ::   Aff a -> Effect (Fiber a)
    - this is good for running an Aff in the main Effect, because it takes an `Aff` and (spawns and) returns a fiber inside an `Effect`. - however, what if we are in an `Aff`, and want to create another fiber?
        - we need `Aff a -> Aff    (Fiber a)`
    forkAff   ::   Aff a -> Aff    (Fiber a)
    forkAff lets us fork another fiber inside an Aff.
    launchAff launches a fiber inside an Effect. In other words, launchAff creates an patient-zero fiber.
--}
