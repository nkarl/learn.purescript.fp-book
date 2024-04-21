module RNG.RandomNumber03 where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (forever)
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
  NOTE: Program Description.
    The program shall generates three float values, each filtered through one of the following independent condition:
    - greater than 0.5
    - less    than 0.5
    - less    than 0.5 and greater than 0.1
    The program shall print these values to the console.
--}

{--
  NOTE: Program Specs.
    This is a good use case for the publisher-subscriber model, which can be written async.
    To use async, we employ `Aff`. Thus, our program's main monad stack starts with Effect <<< Aff.
--}

{--
  NOTE: We start by modeling the program.
    The subscriber and publishers need to communicate.
    We use a Bus as the data-sharing channel between the two types of fiber.
--}

type Bus = BusRW String

{--
  NOTE: Next we compose our monad stack.
    We need to think about how long we should let the program run.
      1. We can let the program run indefinitely. In this case, we just need a Reader to read the Bus.
        - the monad stack becomes (Reader (Aff (Effect)))
      2. We can run the program for just a few iterations to test that it works. In this case,
        - we need a State added to the monad stack.
          - We use State to keep a global count down for how many times we want the program to run.
        - the monad stack becomes (Reader (State (Aff (Effect))))
    We will go with option 1.
--}

type MonadStack a = ReaderT Reader (Aff) a

type Reader = { bus :: Bus }

{--
  NOTE: Next, we model our async fibers.
    We have 4 fibers in total: 3 for publishers, and 1 for subscriber.
      - each publisher fiber will be forked from Aff, and be given a condition.
      - the subscriber will also be forked from Aff.
    We need a function to run the monad stack.
      - This function will take some MonadStack of type a and some Bus and run it, producing some effect at the end.
      - Because we read and write to the bus, we don't actually need to retain any Effect value.
        - Thus we constrain the polymorphic type a to Unit.
--}

runMonadStack :: Reader -> (MonadStack Unit -> Aff Unit)
runMonadStack { bus } =
  void
    <<< forkAff
    <<< flip runReaderT { bus }


{--
  NOTE: Next, we model the publisher and subscriber.
--}

subscribe :: MonadStack Unit
subscribe = forever do
  { bus } <- ask
  s       <- liftAff $ Bus.read bus
  log $ "Logger: " <> s

publish :: String -> (Number -> Boolean) -> MonadStack Unit
publish label predicate = forever do
  { bus } <- ask
  liftAff do
    n <- delayGenerate
    let output = label <> show n
    when (predicate n) $ Bus.write output bus

{--
  NOTE: the publisher is where we need to do the bulk of the work, ie generating a number and checking the predicate.
    We need to model the action of generating a new value.
    We also delay the random generation by 500ms. Otherwise, the console log would get flooded with output.
--}

generateRandom :: Aff Number
generateRandom = liftEffect random

delayGenerate :: Aff Number
delayGenerate = wait *> generateRandom
  where wait = delay (Milliseconds 500.0)

test :: Effect Unit
test = launchAff_ do
  bus <- Bus.make
  let fork = runMonadStack { bus }
  fork $ subscribe
  fork $ flip publish (_ > 0.5) " > 0.5\t\t"
  fork $ flip publish (_ < 0.5) " < 0.5\t\t"
  fork $ flip publish (\x -> x > 0.1 && x < 0.5) " > 0.1 && < 0.5\t"
