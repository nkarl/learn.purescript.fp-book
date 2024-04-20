module RNG.RandomNumber03 where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)

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
    This is a good application for the publisher-subscriber model, which can be written async.
    To use async, we employ `Aff`. Thus, our program's main monad stack starts with Effect <<< Aff.
    Next, we think about how long we should let the program run.
      1. We can let the program run indefinitely. In this case, we just need a Reader to read the Bus.
        - the monad stack becomes Reader (Aff (Effect))
      2. We can run the program for just a few iterations to test that it works. In this case, we need a State and a Reader.
        - the monad stack becomes Reader (State (Aff (Effect)))
--}

{--
  | We start by modeling the program.
  | The subscriber and publishers need to communicate. We use a Bus to share data between the two types.
--}
type Bus = BusRW String

type Reader = { bus :: Bus }

type MonadStack a = ReaderT Reader Aff a

{--
  NOTE: Next, we model our async fibers.
    We have 4 fibers in total: 3 for publishers, and 1 for subscriber.
      - each publisher fiber will be forked from Aff, and be given a condition.
      - the subscriber will also be forked from Aff.
    We need a function to run the monad stack.
      - This function will take some MonadStack type a and some Bus and run it, producing some effect at the end.
      - Because we read and write to the bus, we don't actually need to retain any returned value.
        - Thus we constrain the polymorphic type to Unit.
--}

runMonadStack :: BusRW String -> (MonadStack Unit -> Aff Unit)
runMonadStack bus = void <<< forkAff <<< flip runReaderT { bus: bus }


{--
  NOTE: Next, we model the publisher and subscriber.
--}

publish :: {- MonadStack -} Aff Unit
publish = pure unit

subscribe :: {- MonadStack -} Aff Unit
subscribe = pure unit

test :: Effect Unit
test = launchAff_ do
  bus <- Bus.make
  let fork = runMonadStack bus
  fork $ liftAff subscribe
  fork $ liftAff publish
