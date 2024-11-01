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
import Utils (println)

type State a = Tuple String a

type Process e a = (Failing e) => (String -> Either e (State a))

newtype Action e a = Action (Process e a)

data ErrorMsg
  = EOF
  | InvalidChar String

derive instance genericErrorMsg :: Generic ErrorMsg _

instance showErrorMsg :: Show ErrorMsg where
  show = genericShow

class Failing {- with error -} (e :: Type) where
  eof :: e
  invalidChar :: String -> e

instance failError :: Failing ErrorMsg where
  eof = EOF
  invalidChar = InvalidChar

instance functorAction :: Functor (Action e) where
  -- map f (Action mx) = Action \s -> h $ mx s
  -- where
  -- h = map g
  -- g = map f
  -- map f (Action mx) =
  --   Action \s -> do
  --     let
  --       g = map f
  --       h = map g
  --     h $ mx s
  map f (Action mx) = Action $ ((f <$> _) <$> _) <<< mx

-- apply
instance applyAction :: Apply (Action e) where
  --apply (Action mf) (Action mx) =
  --Action \s -> do
  --Tuple s' f <- mf s
  --Tuple s'' x <- mx s'
  --pure $ Tuple s'' (f x)
  apply = ap

-- applicative
instance applicativeAction :: Applicative (Action e) where
  pure x = Action \s -> pure $ Tuple s x

-- bind
instance bindAction :: Bind (Action e) where
  bind (Action mx) f =
    Action \s -> do
      Tuple s' x <- mx s
      Action g <- pure $ f x
      g s'

-- monad
instance monadAction :: Monad (Action e)

take1char :: forall e. Action e Char
--instance TakeOneChar ErrorMsg where
take1char = Action \s ->
  case (uncons s) of
    Nothing -> Left eof -- :: (Either ErrorMsg _)
    Just { head, tail } -> Right $ Tuple tail head

{-
  NOTE: take some chars from the string
-}
take2chars :: forall e. Action e String
take2chars = do
  c1 <- take1char
  c2 <- take1char
  pure $ fromCharArray [ c1, c2 ]

--take2chars = fromCharArray <$> (take1char <*> take1char)

--take3chars :: (Action ErrorMsg) String
--take3chars = Tuple <$> take1char <*> take1char <*> take1char

{-- helper --}
--unwrap' :: forall e a. Failing e => Action e a -> Process e a
--unwrap' (Action f) = f
--unwrap :: forall a. Action ErrorMsg a -> Process ErrorMsg a
--unwrap = unwrap'
--unwrap (Action f) = f
unwrap' :: forall e a. Failing e => Action e a -> Process e a
unwrap' (Action f) = f

unwrap :: forall a. Action ErrorMsg a -> Process ErrorMsg a
unwrap = unwrap'

fail :: forall e a. (Failing e) => e -> Action e a
fail e = Action $ \_ -> Left e

satisfy :: forall e. (Failing e) => String -> (Char -> Boolean) -> Action e Char
satisfy reason predicate =
  take1char >>= \c ->
    if predicate c then pure c
    else fail $ invalidChar reason -- Left $ invalidChar reason

digit :: forall e. Failing e => Action e Char
digit = satisfy "is not digit" (isDecDigit <<< codePointFromChar)

letter :: forall e. Failing e => Action e Char
letter = satisfy "is not letter" (isAlpha <<< codePointFromChar)

--alphaNum :: forall e. Failing e => Action e Char
--alphaNum =
--  Action \s -> case (unwrap letter s :: Either ErrorMsg _) of
--    Right x -> Right x
--    Left _ -> case unwrap digit s of
--      Left err -> Left err
--      Right y -> Right y
instance altParse :: Alt (Action e) where
  alt p1 p2 =
    Action \s -> case unwrap' p1 s of
      Left _ -> unwrap' p2 s
      Right x -> Right x

alphaNum :: forall e. Failing e => Action e Char
alphaNum = letter <|> digit <|> fail (invalidChar "is not alphaNum")

-- | - replicates the parser `p` some `n` times
-- | - is not memory-efficient
-- | - do we need to replicate the parser (n times)?
-- |    - in the compiled JS output, every call to replicate creates a new tuple record
-- |      with reference to the parser and its generator index.
-- |    - *we do, under the FSM model*. It is in order to track the iterative states of the parser FSM.
-- |    - this seems unnecessarily complex, but how and why?
replicateContextNTimes
  :: forall e a f
   . Traversable f
  => Unfoldable f
  => Int
  -> Action e a
  -> Action e (f a)
replicateContextNTimes n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p) -- replicate the parser `p` some `n` times

parseNTimes :: forall e. Int -> Action e Char -> Action e String
parseNTimes n p = fromCharArray <$> replicateContextNTimes n p

test :: Effect Unit
test = do
  let
    parseSomeDigits = flip parseNTimes $ digit
    parseSomeLetters = flip parseNTimes $ letter
    parseSomeAlphaNumerals = flip parseNTimes $ alphaNum
  println $ "module: Monads.Action.Unit"
  println $ (unwrap (parseSomeDigits 3)) "1234567"
  println $ (unwrap (parseSomeDigits 3)) "abc1234"
  println $ (unwrap (parseSomeLetters 4)) "Freddy"
  println $ (unwrap (parseSomeAlphaNumerals 5)) "a1b2c3d4e5"
  println $ (unwrap (parseSomeAlphaNumerals 10)) "a1b2c3d4e5"
  println $ (unwrap (parseSomeAlphaNumerals 10)) "######" -- input string contains no alpha-numerals

{--
  TODO: review the following functions and graph them 
  - `(unwrap Action) "abcde"` 
  - `Action` is the context for the main parsing action `satify`
  - `satisfy` takes some `predicate` and a String object, and then
    - map `take1char` over the string, one char at a time
    - the char is immediately "piped" to an anonymous function to be checked against the `predicate`
--}
