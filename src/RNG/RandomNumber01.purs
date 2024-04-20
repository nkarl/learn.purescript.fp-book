module RNG.RandomNumber01 where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.State.Trans (StateT, runStateT)
import Data.Foldable (and)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Bus (BusRW)
import Effect.Class (liftEffect)
import Effect.Class as Effect.Class
import Effect.Class.Console (log)
import Effect.Random (random)

{-
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
            - when concurrency is needed, we use `Aff` inside Effect. Aff let us manage multiple `Fiber`, which can be forked from Aff.
  PROGRAM CONSTRUCTION:
    1. [x] generate a random value wrapped in an Aff.
    2. [x] create a monad stack, and
      2.1. [ ] run that monad stack in a fiber.
    3. [ ] create a Bus.
    4. [ ] publish to a Bus.
    5. [ ] subscribe to a Bus.
-}

{-- TYPES & ALIASES --}

type Bus      = BusRW String

type Reader   = { bus :: Bus }

type State    = { count :: Int }

-- Aff Unit at the core

--   FiberM a :: trans  r       m                  a
type FiberMS a = ReaderT Reader (StateT State Aff) a

{-- FUNCTIONS --}

-- | creates a random value wrapped in Aff
affRandom :: Aff Number
affRandom = Effect.Class.liftEffect random -- lift types `Number` from Effect to Aff.

runFiberM :: BusRW String -> (FiberMS Unit -> Aff Unit)-- (Tuple Unit State))
runFiberM bus =
  void
  <<< flip runStateT {count: 10}
  <<< flip runReaderT { bus }

test :: Effect Unit
test = do
  log $ show $ "placeholder"
