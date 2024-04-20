module TickTock.Rep05 where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)

-- | The state of the clock. TICK, TOCK, TICK, TOCK...
-- |
-- | Each option is a mark, denoting that one second has elapsed.
-- | Both together completes a single cycle, with TOCK marking the end of the cycle.
data TickTock
  = TICK
  | TOCK

-- | runs the clock alternating between TICK and TOCK.
run :: AVar TickTock -> Aff Unit
run clock = do
  let
    go :: TickTock -> Aff Unit
    go = \t -> do
      void (AVar.take clock)
      delay (Milliseconds 1000.0)
      AVar.put t clock
  go TICK
  go TOCK
  run clock -- recurs and thus repeats the cycles

-- | Sets the bomb by starting a count down with some amount of clock cycles.
-- |
-- | The bomb goes "BOOM!" if the count down (a differential descent) reaches 0.
setBomb :: AVar TickTock -> (Int -> Aff Unit)
setBomb clock cycles = countDown cycles
  where
  countDown :: Int -> Aff Unit
  countDown c =
    if c <= 0 then
      log "BOOM!!"
    else do
      delay (Milliseconds 500.0) -- otherwise we would unnecessarily exhaust computer resources by recurring every CPU cycle.
      state <- AVar.read clock
      case state of
        TICK -> log "TICK " *> countDown c
        TOCK -> log "TOCK " *> countDown (c - 1)

-- | Runs this module (TickTock.Rep05) as a test program. Sets the bomb with 3 clock cycles.
test :: Effect Unit
test =
  launchAff_ do
    clock <- AVar.empty
    AVar.put TICK clock
    fiberClock <- forkAff $ run      clock
    fiberBomb  <- forkAff $ setBomb  clock 3
    -- join the bomb fiber once it has run its course, and then kill the clock.
    joinFiber fiberBomb
    killFiber (error "Exploded") fiberClock -- stops ticking the clock when the bomb has exploded.
