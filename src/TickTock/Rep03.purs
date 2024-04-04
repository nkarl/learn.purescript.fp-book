module TickTock.Rep03 where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)

{--
  NOTE: the sum type is incredibly powerful in PureScript.
    - It is similar to an enum in OOP, but way less restrictive because it does not need to be Ord unless needed.
--}
data TickTock
  = TICK
  | TOCK

{--
  NOTE: reason about the clock and its cycles.
    - Each clock cycle has a tick and a tock.
      - for each tick, we view the clock, wait/delay for 1000.0 ms and then set the tick again.
    - However, the state of a clock can only be either TICK or TOCK, ie a single state at any given time.
      - that means a tock has to replace a tock, and then tock replaces tick, and so on ad infinitum.
--}
runClock :: AVar TickTock -> Aff Unit
runClock clock = do
  let
    go :: TickTock -> Aff Unit
    go t = do
      void $ AVar.take clock
      delay (Milliseconds 1000.0)
      AVar.put t clock
  go TICK
  go TOCK
  runClock clock

runBomb :: AVar TickTock -> Int -> Aff Unit
runBomb clock cycles = countDown cycles
  where
  countDown :: Int -> Aff Unit
  countDown acc =
    if acc == 0 then
      log "BOOM!!!"
    else do
      delay (Milliseconds 500.0)
      state <- AVar.read clock
      case state of
        TICK -> (log $ "TICK" <> show acc) *> countDown acc
        -- completed a TICK-TOCK cycle, so we decrement the cycle count by 1.
        TOCK -> (log $ "TOCK" <> show (acc - 1)) *> countDown (acc - 1)

test :: Effect Unit
test =
  launchAff_ do
    clock <- AVar.empty -- set the clock to empty
    AVar.put TICK clock
    fiberClock <- forkAff $ runClock clock
    fiberBomb <- forkAff $ runBomb clock 3
    joinFiber fiberBomb
    killFiber (error "Exploded") fiberClock
