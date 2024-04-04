module Ch23a where

import Prelude
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, error, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)

data TickTock
  = Tick
  | Tock

derive instance eqTikTok :: Eq TickTock

-- | a single cycle combined from tick and tock
clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
  -- tock
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tock ttAVar
  -- tick
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tick ttAVar
  -- recurse
  clock ttAVar

bomb :: (AVar TickTock) -> Int -> Aff Unit
bomb ttAVar deadline = countDown deadline
  where
  countDown :: Int -> Aff Unit
  countDown acc =
    if acc == 0 then
      log "BOOM!!!"
    else do
      delay (Milliseconds 500.0) -- ?why do we need to delay by a quarter of a cycle here
      tt <- AVar.read ttAVar
      case tt of
        Tick -> (log $ "Tick " `append` show acc) *> countDown acc
        Tock -> (log $ "Tock " `append` show acc) *> countDown (acc - 1)

test :: Effect Unit
test =
  launchAff_ do
    ttAVar <- AVar.empty
    -- clock and bomb run independently
    clockFiber <- forkAff $ clock ttAVar
    bombFiber <- forkAff $ bomb ttAVar 3
    AVar.put Tick ttAVar
    joinFiber bombFiber
    killFiber (error "Exploded") clockFiber
