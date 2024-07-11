module MooreMachine.Unit where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

import Data.Profunctor (class Profunctor)

print :: forall a (m ∷ Type -> Type). MonadEffect m => Show a => a -> m Unit
print = log <<< show

type Output s b = s -> b

type Step   s a = s -> a -> s

data FSM s a b
  = FSM s (s -> b) (s -> a -> s)

instance profunctorFSM :: Profunctor (FSM s) where
  dimap :: forall a b c d. (c -> a) -> (b -> d) -> FSM s a b -> FSM s c d
  dimap f g (FSM s0 output step) = FSM s0 (g <<< output) (\s -> step s <<< f)
-- FSM s a b
-- FSM s c d

-- g                :: b -> d
-- output           :: s -> b
-- g . output       :: s -> d

-- f                :: c -> a
-- step             :: s -> a -> s
-- step s           :: a -> s
-- step s . f       :: c -> s
-- \s -> step s . f :: s -> (c -> s)

-- state S
data Oven = Off | Bake | Idling
-- input \Sigma
data Signal = ButtonBake | ButtonOff | TooHot | TooCold 
-- output \Lambda
data Heat = Hot | Cold

-- checks a subset of the Cartesian product of S and \Sigma 
stepFn :: Oven -> Signal -> Oven
stepFn x y = go $ Tuple x y
  where
    go = case _ of
      Tuple Off     ButtonBake  -> Bake
      Tuple Bake    ButtonOff   -> Off
      Tuple Bake    TooHot      -> Idling
      Tuple Idling  TooCold     -> Bake
      Tuple Idling  ButtonOff   -> Off
      Tuple s       _           -> s

outputFn :: Oven -> Heat
outputFn = case _ of
  Off     -> Cold
  Bake    -> Hot
  Idling  -> Cold

test :: Effect Unit
test = do
  print "placeholder"
