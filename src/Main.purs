module Main where

import Prelude
import Effect (Effect)
--import Ch5 as Ch5
--import Ch6 as Ch6
--import Ch7a as Ch7a
--import Ch19 as Ch19
--import Maybe.Maybe07 as Maybe07
--import Either.Either01 as Either01
--import Parser.Applicative as ApplicativeParser
--import Parser.Monadic01 as MonadicParser
--import Reader.Monadic01 as ReaderMonad
--import Ch23a as Ch23a
import TickTock.Rep05 as TickTock.Rep05

main :: Effect Unit
main = do
  --Ch5.test
  --Ch19.testMaybe
  --Ch19.testEither
  --MyMaybe.test
  --AnotherMaybe.test
  --Maybe07.test
  --Either01.test
  --ApplicativeParser.test
  --MonadicParser.test
  --ReaderMonad.test
  --Ch23a.test
  TickTock.Rep05.test
