module StateMachiens.Moore where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.String (length)
import Data.Profunctor (class Profunctor, dimap)
import Effect (Effect)

import Utils (print)

{--
s        :: *            -- the type (finite set) of states $S_0$
s0       :: s            -- an initial state of type `s`
a        :: *            -- the type of input  $\Sigma$
c        :: *            -- the type of output $\Lambda$
stepT    :: s -> a -> s  -- morphism $T$, taking a state and an input, producing a new state
extractG :: s -> c       -- morphism $G$, tating a state, producing an output

x        :: a            -- an input value of type `a`
--}
type ExtractG s c
  = s -> c

type StepT s a
  = s -> a -> s

--data FSM s a c
--  = FSM s (ExtractG s c) (StepT s a)
data FSM s a c
 = FSM s (s -> c) (s -> a -> s)

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
      - input  of `stepT`    in $FSM_1$ is mapped to output of `stepT`   in $FSM_2$ 

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

-- because that all functors in PureScript are endo-functors,
-- we can rewrite the signature with just `a` as below
-- runFoldL :: forall s a f. Foldable f => FSM s a a -> f a -> a
runFoldL :: forall s a b f. Foldable f => FSM s a b -> f a -> b
runFoldL (FSM s0 extract transition) = extract <<< foldl transition s0

bareFoldL :: forall a f. Foldable f => Semiring a => f a -> a
bareFoldL = foldl (+) zero

-- NOTE: in this example, the polymorphic types in the involved actions
-- are constrained to the same Semiring type.
addr :: forall a. Semiring a => FSM a a a
addr = FSM zero identity (+)

-- takes a `Foldable String` and sum up the lengths of all strings in it
--         FSM s   b      c
-- addr :: FSM Int Int    Int
--         FSM s   a      d
sizer   :: FSM Int String Int
sizer = dimap length identity addr


{--
  TODO:
    1. [x] review the FSM and profunctor
    2. [x] aim for a _working_ understanding
    3. [x] implement FSM for `foldL`
    4. [x] write unit tests
    5. [ ] review and bolster my understanding in the context of the unit tests
--}

-- test unit
test :: Effect Unit
test = do
  print "test: MooreMachine"
  print $ 6 == (bareFoldL [1, 2, 3])
  print $ 6 == (runFoldL addr [1, 2, 3])
  print $ 6.0 == (runFoldL addr [1.0, 2.0, 3.0])
  print $ 3 == (runFoldL sizer ["a", "bb"])


{--
  NOTE: some notes
  - an FSM is a computational context
  - `addr` is a function that is wrapped in side a FSM context
  - `runFoldL addr`
    - performs a `foldl` with the function `addr` over a Foldable `f`, and then
    - invokes `extract` in the FSM and retrieve the output
  - a `dimap` is like `map` but more complex, and requires more careful attention
  - a `dimap` is a pattern we can use when we have to map two actions into FSM context
--}
