module Main where

import Prelude

import Effect (Effect)

--import Ch5 as Ch5
--import Ch6 as Ch6
--import Ch7a as Ch7a
--import Ch7b as Ch7b
--import CSV.Rep04 as CSV.Rep04
--import Ch19 as Ch19
--import Maybe.Maybe07 as Maybe07
--import Either.Either01 as Either01
--import Parser.Applicative01 as Parser.Applicative01
--import Parser.Monadic02 as Parser.Monadic02
--import Reader.Monadic01 as ReaderMonad
--import Ch23a as Ch23a
--import TickTock.Rep05 as TickTock.Rep05
--import Ch23b as Ch23b
--import EchoServer.Ch25a as EchoServer.Ch25a
--import FFI.JoinPath as FFI.JoinPath
import RNG.RandomNumber01 as RandomNumber01

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
  RandomNumber01.test
