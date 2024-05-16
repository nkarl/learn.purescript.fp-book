module RNG.RandomNumber02 where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Control.Monad.State.Trans (StateT, get, modify_, runStateT)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, launchAff_)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

{--
  PROGRAM DESCRIPTION:
  - The program will generate three random float values.
  - Each value is filtered through one of these three independent boolean conditions:
    - greater than 0.5
    - lesss than 0.5
    - greater than 0.1
  - the program will output these values on the console.
--}
{--
  PROGRAM SPECS:
  - This is a good use case for the publisher-subscriber model, ie async effects.
  - We use Aff:
    - spawn 3 fibers, each for one publisher with one of the 3 filter conditions.
    - spawn 1 fiber for the subscriber.
  - We use a BusRW to record and share data between the pubs and sub.
  - The program flow:
    - The publishers writes the randomized values into the bus.
    - The subscriber reads the bus and is responsible for the console ouput, effectively a logger.
  - More importantly, we will only run the program for a few iterations. This means that we
    also need a global count down. We use a State to capture this.
  - We compose a MonadStack with Effect $ Aff $ State $ Reader in order run the computation stack.
  - We wraps the function that generates a new float value to slow it down by 500 ms.
--}
-- | the data bus; has Read-Write capabilities. Accessed by both the 3 publishers and the lone subscriber.
type Bus
  = BusRW String

-- | a type alias for the data-bus.
type Reader
  = { bus :: Bus }

-- | a type alias for the global count down.
type State
  = { count :: Int }

-- | our program's monad stack.
type MonadStack a
  = ReaderT Reader (StateT State Aff) a

-- | the function to kick start the monad stack.
runMonadStack :: BusRW String -> (MonadStack Unit -> Aff Unit)
runMonadStack bus =
  void
    <<< forkAff
    <<< flip runStateT { count: 10 }
    <<< flip runReaderT { bus }

-- | the subscribe fiber.
subcribe :: MonadStack Unit
subcribe = do
  { bus } <- ask
  s <- liftAff $ Bus.read bus
  log $ "Logger: " <> s
  subcribe

-- | the publish fiber.
publish :: (Number -> Boolean) -> MonadStack Unit
publish predicate = do
  { count } <- get
  unless (count <= 0) do
    { bus } <- ask
    liftAff do
      n <- delayRandom
      let
        output = show n
      when (predicate n) $ Bus.write output bus
    modify_ _ { count = count - 1 }
    publish predicate

delayRandom :: Aff Number
delayRandom = wait *> generateRandom
  where
  wait = delay (Milliseconds 500.0) -- combines two functors `f a` and `f b`, keeping only result of `f b`.

generateRandom :: Aff Number
generateRandom = liftEffect random

test :: Effect Unit
test =
  launchAff_ do
    bus <- Bus.make
    let
      forkFiber = runMonadStack bus

      logger = forkFiber $ subcribe
    logger
    forkFiber $ publish (_ > 0.5)
    forkFiber $ publish (_ < 0.5)
    forkFiber $ publish (_ > 0.1)
