module RNG.RandomNumber01 where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array ((..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Class as Effect.Class
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
    3. [ ] create a Bus.
    4. [ ] publish to a Bus.
    5. [ ] subscribe to a Bus.
--}

{-- TYPES & ALIASES --}

type Bus      = BusRW String

type Reader   = { bus :: Bus }

type State    = { count :: Int }

-- Aff Unit at the core

--   MonadStack a :: trans  r       m                 a
type MonadStack a = ReaderT Reader (StateT State Aff) a

{-- FUNCTIONS --}

-- | creates a random value wrapped in Aff
affRandom :: Aff Number
affRandom = Effect.Class.liftEffect random -- lift types `Number` from Effect to Aff.
-- I guess my confusion here is why I can't use `lift`.


runMonadStack :: BusRW String -> (MonadStack Unit -> Aff Unit)-- (Tuple Unit State))
runMonadStack bus =
  void
  <<< forkAff
  <<< flip runStateT {count: 10}
  <<< flip runReaderT { bus }

logger :: {- Aff -} MonadStack Unit
logger = do
  { bus } <- ask
  s <- lift $ lift $ Bus.read bus
  log $ "Logger: " <> s
  pure unit
-- `liftEffect` is different from `lift`. liftEffect lift from Effect to Aff.
-- On the other hand, lift pulls the lower layer to the upper layer, one layer at a time.

randomGenerator :: (Number -> Boolean) -> {- Aff -} MonadStack Unit
randomGenerator predicate = pure unit

test :: Effect Unit
test = launchAff_ do
  bus <- Bus.make
  let forkFiber = runMonadStack bus
  forkFiber $ logger
  forkFiber $ randomGenerator (_ > 0.5)
  forkFiber $ randomGenerator (_ < 0.5)
  forkFiber $ randomGenerator (_ > 0.1)
