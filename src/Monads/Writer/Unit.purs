module Monads.Writer.Unit where

import Prelude
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Utils (println)

newtype Writer s a
  = Writer (Tuple a s)

instance functorWriter :: Functor (Writer s) where
  map f (Writer (Tuple x l)) = Writer (Tuple (f x) l)

instance applyWriter :: Monoid s => Apply (Writer s) where
  --apply (Writer (Tuple f s1)) (Writer (Tuple x s2)) = Writer (Tuple (f x) (s1 <> s2))
  apply = ap

instance applicativeWriter :: Monoid s => Applicative (Writer s) where
  pure x = Writer (Tuple x mempty)

instance bindWriter :: Monoid s => Bind (Writer s) where
  bind (Writer (Tuple x s1)) f = f x # \(Writer (Tuple y s2)) -> Writer (Tuple y (s1 <> s2))

instance monadWriter :: Monoid s => Monad (Writer s)

{-
  NOTE: Writer Helper Functions
-}
tell :: forall s. s -> Writer s Unit
tell s = Writer (Tuple unit s)

listen :: forall a s. Writer s a -> Writer s (Tuple a s)
listen (Writer t) = do
  let
    Tuple _ s = t
  Writer $ Tuple t s

pass :: forall a s. Writer s (Tuple a (s -> s)) -> Writer s a
pass (Writer t) = do
  let
    Tuple t' s = t
    Tuple value f = t'
  Writer $ Tuple value (f s)

doNothingWithLog :: Writer (Array String) Int
doNothingWithLog = do
  tell [ "We did nothing" ]
  --Writer (Tuple 0 ["We did nothing"])
  pure 0

test :: Effect Unit
test = do
  println $ "placeholder"
