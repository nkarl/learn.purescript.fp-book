module Monads.Parser.Unit where

import Prelude
import Control.Plus (class Alt, (<|>))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicate, none)
import Effect (Effect)
import Utils (print)

type State a
  = Tuple String a

type Process e a
  = (Failure e) => (String -> Either e (State a))

newtype Parse e a
  = Parse (Process e a)

data ErrorMsg
  = EOF
  | InvalidChar String

derive instance genericErrorMsg :: Generic ErrorMsg _

instance showErrorMsg :: Show ErrorMsg where
  show = genericShow

class Failure (e :: Type) where
  eof :: e
  invalidChar :: String -> e

instance failError :: Failure ErrorMsg where
  eof = EOF
  invalidChar = InvalidChar

instance functorAction :: Functor (Parse e) where
  --map f (Parse mx) = Parse \s -> h $ mx s
  --where
  --h = map g
  --g = map f
  map f (Parse mx) =
    Parse \s -> do
      let
        g = map f

        h = map g
      h $ mx s

-- apply
instance applyAction :: Apply (Parse e) where
  --apply (Parse mf) (Parse mx) =
  --Parse \s -> do
  --Tuple s' f <- mf s
  --Tuple s'' x <- mx s'
  --pure $ Tuple s'' (f x)
  apply = ap

-- applicative
instance applicativeAction :: Applicative (Parse e) where
  pure x = Parse \s -> pure $ Tuple s x

-- bind
instance bindAction :: Bind (Parse e) where
  bind (Parse mx) f =
    Parse \s -> do
      Tuple s' x <- mx s
      Parse g <- pure $ f x
      g s'

-- monad
instance monadAction :: Monad (Parse e)

take1char :: forall e. Parse e Char
--instance TakeOneChar ErrorMsg where
take1char =
  Parse \s -> case uncons s of
    Nothing -> Left eof -- :: (Either ErrorMsg _)
    Just { head, tail } -> Right $ Tuple tail head

{-
  NOTE: take some chars from the string
-}
take2chars :: (Parse ErrorMsg) String
take2chars = do
  c1 <- take1char
  c2 <- take1char
  pure $ fromCharArray [ c1, c2 ]

{-- helper --}
--unwrap' :: forall e a. Failure e => Parse e a -> Process e a
--unwrap' (Parse f) = f
--unwrap :: forall a. Parse ErrorMsg a -> Process ErrorMsg a
--unwrap = unwrap'
--unwrap (Parse f) = f
unwrap' :: forall e a. Failure e => Parse e a -> Process e a
unwrap' (Parse f) = f

unwrap :: forall a. Parse ErrorMsg a -> Process ErrorMsg a
unwrap = unwrap'

fail :: forall e a. (Failure e) => e -> Parse e a
fail e = Parse $ \_ -> Left e

satisfy :: forall e. (Failure e) => String -> (Char -> Boolean) -> Parse e Char
satisfy reason predicate =
  take1char -- returns a `Parser` context which wraps a (rest, c)
    >>= \c ->
        if predicate c then pure c else fail $ invalidChar reason -- Left $ invalidChar reason

digit :: forall e. Failure e => Parse e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: forall e. Failure e => Parse e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

--alphaNum :: forall e. Failure e => Parse e Char
--alphaNum =
--  Parse \s -> case (unwrap letter s :: Either ErrorMsg _) of
--    Right x -> Right x
--    Left _ -> case unwrap digit s of
--      Left err -> Left err
--      Right y -> Right y
instance altParse :: Alt (Parse e) where
  alt p1 p2 =
    Parse \s -> case unwrap' p1 s of
      Left _ -> unwrap' p2 s
      Right x -> Right x

alphaNum :: forall e. Failure e => Parse e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

{--
  - replicates the parser `p` some `n` times
  - is not memory-efficient
    - do we need to replicate the parser (n times)?
        - in the compiled JS output, every call to replicate creates a new tuple record
        with reference to the parser and its generator index.
        - we do, under the FSM model. It is in order to track the iterative states of the parser FSM.
          - how this is seems unnecessarily complex, but how and why?
--}
replicateContextNTimes ::
  forall e a f.
  Traversable f =>
  Unfoldable f =>
  Int -> Parse e a -> Parse e (f a)
replicateContextNTimes n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p) -- replicate the parser `p` some `n` times

parseNTimes :: forall e. Int -> Parse e Char -> Parse e String
parseNTimes n p = fromCharArray <$> replicateContextNTimes n p

{--
  TODO: review the following functions and graph them 
    - `(unwrap Parser) "abcde"` 
    - `Parser` is the context for the main parsing action `satify`
      - `satisfy` takes some `predicate` and a String object, and then
      - interally map `take1char` over the string, one char at a time
      - the char is immediately "piped" to an anonymous function to be checked for validity
--}
test :: Effect Unit
test = do
  let
    parseSomeDigits = flip parseNTimes $ digit
    parseSomeLetters = flip parseNTimes $ letter
    parseSomeAlphaNumerals = flip parseNTimes $ alphaNum
  print $ "module: Monads.Parser.Unit"
  print $ (unwrap (parseSomeDigits 3)) "1234567"
  print $ (unwrap (parseSomeDigits 3)) "abc1234"
  print $ (unwrap (parseSomeLetters 4)) "Freddy"
  print $ (unwrap (parseSomeAlphaNumerals 5)) "a1b2c3d4e5"
  print $ (unwrap (parseSomeAlphaNumerals 10)) "a1b2c3d4e5"
  print $ (unwrap (parseSomeAlphaNumerals 10)) "######" -- input string contains no alpha-numerals
