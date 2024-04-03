module Main where

import Prelude
import Effect (Effect)
--import Ch5 as Ch5
--import Ch6 as Ch6
--import Ch7a as Ch7a
--import Ch19 as Ch19
--import Maybe.Maybe01 as Maybe01
--import Maybe.Maybe02 as Maybe02
--import Maybe.Maybe03 as Maybe03
--import Maybe.Maybe04 as Maybe04
--import Maybe.Maybe05 as Maybe05
--import Maybe.Maybe06 as Maybe06
--import Maybe.Maybe07 as Maybe07
--import Either.Either01 as Either01
--import Parser.Applicative as ApplicativeParser
--import Parser.Monadic01 as MonadicParser
--import Reader.Monadic01 as ReaderMonad
import Ch23a as Ch23a

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
  Ch23a.test
