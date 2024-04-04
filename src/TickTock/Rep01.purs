module TickTock.Rep01 where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)

data TickTock
  = Tick
  | Tock

newtype ClockState
  = Avar TickTock

runClock :: AVar TickTock -> Aff Unit
runClock state = do
  void $ AVar.take state
  delay (Milliseconds 1000.0)
  AVar.put Tick state
  void $ AVar.take state
  delay (Milliseconds 1000.0)
  AVar.put Tock state
  runClock state

runBomb :: AVar TickTock -> Int -> Aff Unit
runBomb clock cycles = countDown cycles
  where
  countDown :: Int -> Aff Unit
  countDown acc =
    if acc == 0 then
      log "BOOM!!"
    else do
      delay (Milliseconds 500.0)
      state <- AVar.read clock
      case state of
        Tick -> (log ("Tick " <> show acc)) *> countDown acc
        Tock -> (log ("Tock " <> show acc)) *> countDown (acc - 1)

test :: Effect Unit
test =
  launchAff_ do
    clock <- AVar.empty
    fiberClock <- forkAff (runClock clock)
    fiberBomb <- forkAff (runBomb clock 3)
    AVar.put Tick clock
    joinFiber fiberBomb
    killFiber (error "Exploded") fiberClock
