module RNG.RandomNumber01 where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Trans (StateT, get, modify_, runStateT)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, launchAff_)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

{--
  PROGRAM SPECS:
    - 3 broadcasters
      - one checking if a random number is: larger than 0.5 ( > 0.5)
      - one checking if a random number is: less   than 0.5 ( < 0.5)
      - one checking if a random number is: larger than 0.1 ( > 0.1)
    - 1 subscriber
      - reads the global bus and logs whatever string in the bus.
    - we use Aff to accomodate the 4 fibers of pubs and sub.
      - Thus Aff is the working base (async portion of the program), just above Effect.
        - NOTE:
          - every program starts at `Effect` as absolute base.
            - when concurrency is needed, we "launch" an `Aff` that eventually returns Effect.
            - Aff lets us manage multiple `Fiber`, which might be forked from Aff.

  PROGRAM CONSTRUCTION:
    1. [x] generate a random value wrapped in an Aff.
    2.a. [x] create a monad stack, and
    2.b. [x] run that stack in a fiber (not in Aff).
    3. [x] create a Bus.
    4. [ ] publish to a Bus: write
    5. [x] subscribe to a Bus: read
--}
{-- TYPES & ALIASES --}
type Bus
  = BusRW String

type Reader
  = { bus :: Bus }

type State  {-- we use a state to prevent the program from running indefinitely. we only need a few iterations to demonstrate the point of this programmatic lession in using fibrous pubs and subs. --}
  = { count :: Int }

-- Aff Unit at the core
type MonadStack a
  -- r              m                 a
  = ReaderT Reader (StateT State Aff) a

{-- FUNCTIONS --}
{--
  | creates a random value wrapped in Aff.
  |
  | NOTE: I guess my confusion here is I don't know when to use lift vs liftEffect.
  | - lift is usually part of a transformer API (ie a monad stack).
  |   - the action is implemented for each layer in the stack (excluding Effect and Aff).
  | - liftEffect is more specific; it is constrained by the MonadEffect typeclass.
  |   - liftEffect transforms some Effect <a> to any monadic layer <m> in the stack.
  | - liftAff transforms some Aff <a> to any monadic layer <m> in the stack.
  | - Effect and Aff were given special attention because they are the lowest levels.
--}
affRandom :: Aff Number
--affRandom = Effect.Class.liftEffect random -- lift types `Number` from Effect to Aff.
affRandom = liftEffect random -- lift types `Number` from Effect to Aff.

delayRandom :: Aff Number
delayRandom = delay (Milliseconds 1000.0) *> affRandom

runMonadStack :: BusRW String -> (MonadStack Unit -> Aff Unit) -- (Tuple Unit State))
runMonadStack bus =
  void
    <<< forkAff
    <<< flip runStateT { count: 10 }
    <<< flip runReaderT { bus }

logger :: MonadStack {- Aff -} Unit
logger = forever do
    { bus } <- ask
    -- Bus.read runs in Aff, but we need it to run in a MonadStack.
    -- therefore, we use `liftAff` to pull it to the MonadStack layer.
    s <- liftAff $ Bus.read bus
    log $ "Logger: " <> s
    --logger

randomGenerator :: String -> (Number -> Boolean) -> MonadStack Unit
randomGenerator label pred = do
  { count } <- get
  unless (count <= 0) do
    { bus } <- ask
    -- the next do block returns an Aff, which needs to be lifted to MonadStack
    liftAff do {-- :: Aff -- unless lifted --}
      n <- delayRandom
      let
        output = "Found a value that is " <> label <> " (" <> show n <> ")"
      when (pred n) $ Bus.write output bus
    modify_ _ { count = count - 1 } -- NOTE: 2nd do-block; DOES terminate the program
    randomGenerator label pred
  --modify_ _ { count = count - 1 } -- NOTE: 1st do-block; DOES NOT terminate the program
  --randomGenerator label pred

test :: Effect Unit
test =
  launchAff_ do
    bus <- Bus.make
    let
      forkFiber = runMonadStack bus
    forkFiber $ logger
    forkFiber $ randomGenerator "> 0.5\t" (_ > 0.5)
    forkFiber $ randomGenerator "< 0.5\t" (_ < 0.5)
    forkFiber $ randomGenerator "> 0.1\t" (_ > 0.1)
