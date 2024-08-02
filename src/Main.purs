module Main where

import Prelude
import Effect (Effect)
--import Ch19 as Ch19
import Monads.Writer.Unit as Monads.Writer.Unit

--import Maybe.Maybe07 as Maybe07
--import Either.Either01 as Either01
--import Parser.Applicative01 as Parser.Applicative01
--import Parser.Monadic02 as Parser.Monadic02
--import Monads.Reader.Unit as ReaderMonad
--import Ch23a as Ch23a
--import TickTock.Rep05 as TickTock.Rep05
--import Ch23b as Ch23b
--import EchoServer.Ch25a as EchoServer.Ch25a
--import FFI.JoinPath as FFI.JoinPath
--import RNG.RandomNumber01 as RandomNumber01
--import RNG.RandomNumber02 as RandomNumber02
--import RNG.RandomNumber03 as RandomNumber03
--import Foldable.FoldableList as Foldable.FoldableList
--import Foldable.Foldable as Foldable.Foldable
--import Maybe.Maybe08 as Maybe.Maybe08
--import StateMachines.Moore as StateMachines.Moore
--import Trees.Binary as Trees.Binary
--import Monads.Stack.Transformer as Monads.Stack.Transformer
main :: Effect Unit
main = do
  --Ch5.test
  --Ch7b.test
  --CSV.Rep04.test
  --Ch19.testMaybe
  --Ch19.testEither
  --MyMaybe.test
  --AnotherMaybe.test
  --Maybe07.test
  --Either01.test
  --Parser.Applicative01.test
  --Parser.Monadic02.test
  --ReaderMonad.test
  --Ch23a.test
  --TickTock.Rep05.test
  --Ch23b.test
  --EchoServer.Ch25a.test
  --FFI.JoinPath.test
  --RandomNumber03.test
  --Foldable.FoldableList.test
  --Foldable.Foldable.test
  --Maybe.Maybe08.test
  --StateMachines.Moore.test
  --Trees.Binary.test
  --Monads.Stack.Transformer.test
  Monads.Writer.Unit.test
