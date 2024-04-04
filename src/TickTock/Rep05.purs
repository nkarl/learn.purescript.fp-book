module TickTock.Rep05 where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)

data TickTock
  = TICK
  | TOCK

tick :: AVar TickTock -> Aff Unit
tick clock = do
  let
    go t = do
      void (AVar.take clock)
      delay (Milliseconds 1000.0)
      AVar.put t clock
  go TICK
  go TOCK
  tick clock

bomb :: AVar TickTock -> Int -> Aff Unit
bomb clock cycles = countDown cycles
  where
  countDown :: Int -> Aff Unit
  countDown acc =
    if acc == 0 then
      log "BOOM!!"
    else do
      delay (Milliseconds 500.0)
      state <- AVar.read clock
      case state of
        TICK -> log "TICK " *> countDown acc
        TOCK -> log "TOCK " *> countDown (acc - 1)

test :: Effect Unit
test =
  launchAff_ do
    clock <- AVar.empty
    AVar.put TICK clock
    fiberTick <- forkAff $ tick clock
    fiberBomb <- forkAff $ bomb clock 3
    joinFiber fiberBomb
    killFiber (error "Exploded") fiberTick
