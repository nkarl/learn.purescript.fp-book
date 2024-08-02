module Monads.State.Unit where

import Prelude
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

newtype State s a
  = State (s -> Tuple a s)

{-- NOTE:
      1. receives a State variable `s`
      2. runs the State `cx` on `s`
      3. passes the result from #2 to a lambda which
        - unwraps the result from #2, and
        - produce a new Tuple from
          - (f x)
          - the new state `s'`
--}
instance functorState :: Functor (State s) where
  map f (State cx) = State \s -> cx s # \(Tuple x s') -> Tuple (f x) s'

instance applyState :: Apply (State s) where
  apply (State cf) (State cx) = State \s -> cf s # \(Tuple f s') -> cx s' # \(Tuple x s'') -> Tuple (f x) s''

instance applicativeState :: Applicative (State s) where
  pure x = State \s -> Tuple x s

unwrap :: forall s a. State s a -> (s -> Tuple a s)
unwrap (State x) = x

-- cx :: s -> Tuple a s
-- cx s :: Tuple a s
-- f :: a -> State s b
-- f x :: State s b
-- unwrap f x :: s -> Tuple b s'
-- unwrap (f x) $ s' :: Tuple b s'
instance bindState :: Bind (State s) where
  -- bind :: forall a b. State s a -> (a -> State s b) -> State s b
  bind (State cx) f = State \s -> (cx s) # \(Tuple x s') -> unwrap (f x) $ s' -- # \(Tuple y s'') -> Tuple y s''

get :: forall s. State s s
get = State \s -> Tuple s s

put :: forall s. s -> State s Unit
put s = State \_ -> Tuple unit s

modify :: forall s. (s -> s) -> State s s
modify f = State \s -> let s' = f s in Tuple s' s'

modify_ :: forall s. (s -> s) -> State s Unit
modify_ f = State \s -> Tuple unit (f s)

instance monadState :: Monad (State s)

test :: Effect Unit
test = do
  log $ show $ "monads.state.state01 placeholder"
