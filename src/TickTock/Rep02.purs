module TickTock.Rep02 where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)

data TickTock
  = TICK
  | TOCK

runClock :: AVar TickTock -> Aff Unit
runClock clock = do
  void $ AVar.take clock
  delay (Milliseconds 1000.0)
  AVar.put TICK clock
  void $ AVar.take clock
  delay (Milliseconds 1000.0)
  AVar.put TOCK clock
  runClock clock

runBomb :: AVar TickTock -> Int -> Aff Unit
runBomb clock cycles = countDown cycles
  where
  countDown :: Int -> Aff Unit
  countDown acc =
    if acc == 0 then
      log "BOOM!!"
    else do
      state <- AVar.read clock
      case state of
        TICK -> (log $ "Tick ") *> countDown acc
        TOCK -> (log $ "Tock ") *> countDown (acc - 1)

test :: Effect Unit
test =
  launchAff_ do
    clock <- AVar.empty
    clockFiber <- forkAff $ runClock clock
    bombFiber <- forkAff $ runBomb clock 3
    AVar.put TICK clock
    joinFiber bombFiber
    killFiber (error "Exploded") clockFiber
