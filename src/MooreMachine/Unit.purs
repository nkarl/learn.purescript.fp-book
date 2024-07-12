module MooreMachine.Unit where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

import Data.Profunctor (class Profunctor)

{--
s        :: *            -- the type (finite set) of states $S_0$
s0       :: s            -- an initial state of type `s`
a        :: *            -- the type of input  $\Sigma$
c        :: *            -- the type of output $\Lambda$
stepT    :: s -> a -> s  -- morphism $T$, taking a state and an input, producing a new state
extractG :: s -> c       -- morphism $G$, tating a state, producing an output

x        :: a            -- an input value of type `a`
--}

type ExtractG s c = s -> c
type StepT    s a = s -> a -> s

--data FSM s a c
-- = FSM s (s -> c) (s -> a -> s)
data FSM s a c
  = FSM s (ExtractG s c) (StepT s a)

instance profunctorFSM :: Profunctor (FSM s) where
  dimap :: forall a b c d. (a -> b) -> (c -> d) -> FSM s b c -> FSM s a d
  dimap f g (FSM s0 extractG stepT) = FSM s0 (g <<< extractG) (\s -> stepT s <<< f)

{--
  NOTE: in mapping between the 2 machine states:  from (FSM s b c) 
                                                  to   (FSM s a d),
  we have 2 action variants:
    - contra-variant: b -> a
      - output of `extractG` in $FSM_1$ is mapped to input of `extractG` in $FSM_2$
    - co-variant    : c -> d
      - input of `stepT`     in $FSM_1$ is mapped to output of `stepT`   in $FSM_2$ 

  NOTE: the initial state $s_0$ is preserved and passed on across machine states.
-}

-- FSM s b c :: FSM s (s -> c) (s -> b -> s)
-- FSM s a d :: FSM s (s -> d) (s -> a -> s)

-- extractFn          :: s -> c -- Extract
-- g                  :: c -> d
-- g . extractFn      :: s -> d

-- f                  :: a -> b
-- stepFn             :: s -> b -> s -- Step
-- stepFn s           :: b -> s
-- stepFn s . f       :: a -> s
-- \s -> stepFn s . f :: s -> (a -> s)

-- state S
data Oven = Off | Bake | Idling
-- input \Sigma
data Signal = ButtonBake | ButtonOff | TooHot | TooCold 
-- extractFn \Lambda
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

extractFn :: Oven -> Heat
extractFn = case _ of
  Off     -> Cold
  Bake    -> Hot
  Idling  -> Cold

{--
  TODO:
    1. [x] review the FSM and profunctor
    2. [x] aim for a _working_ understanding
    3. [ ] implement FSM for `foldL`
    4. [ ] write unit tests
    5. [ ] review and bolster my understanding in the context of the unit tests
--}

print :: forall a (m ∷ Type -> Type). MonadEffect m => Show a => a -> m Unit
print = log <<< show

test :: Effect Unit
test = do
  print "placeholder"
