module Reader.Unit where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

-- is a context for a function
newtype Reader r a
  = Reader (r -> a)

instance functorReader :: Functor (Reader r) where
  --map f (Reader g) = Reader \r -> f $ g r
  map f (Reader g) = Reader \r -> (f <<< g) r

--cx   :: Reader r  a
--cx r ::  a
--cf   :: Reader (r) (a -> b)
--cf r :: (a -> b)
instance applyReader :: Apply (Reader r) where
  --apply :: forall a b. Reader r (a -> b) -> Reader r a -> Reader r b
  --apply (Reader cf) (Reader cx) = Reader \r -> cf r $ cx r
  --apply (Reader cf) (Reader cx) = Reader \r -> (cf r) <<< cx $ r
  apply (Reader cf) (Reader cx) = Reader \r -> (cf <*> cx) r

instance applicativeReader :: Applicative (Reader r) where
  --pure x = Reader \_ -> x
  pure = Reader <<< const

--                      f           ::       a -> Reader r b
--                          cx      :: (r -> a)
--                          cx r    ::       a
--                      f $ cx r    ::            Reader r b
-- Reader \r ->         f $ cx r    :: Reader r  (Reader r b)
--                                     ^
--                                  this is where we can use `join`
--                                  however we desire something leaner
--              unwrap (f $ cx r)   ::                r -> b
--              unwrap (f $ cx r) r ::                     b
-- Reader \r -> unwrap (f $ cx r) r :: Reader r            b
instance bindReader :: Bind (Reader r) where
  --bind :: forall a b. Reader r a -> (a -> Reader r b) -> Reader r b
  --bind (Reader cx) f = join $ Reader \r -> f $ cx r
  --bind (Reader cx) f = Reader \r -> runReader (f $ cx r) r
  bind (Reader cx) f = Reader \r -> (unwrap <<< transform) r r
    where
    unwrap (Reader c) = c

    transform = f <<< cx

runReader :: forall a r. Reader r a -> r -> a
runReader (Reader f) = f

instance monadReader :: Monad (Reader r)

ask :: forall r. Reader r r
ask = Reader $ identity

asks :: forall a r. (r -> a) -> Reader r a
asks f = Reader \r -> f r

test :: Effect Unit
test = do
  log $ show $ "placeholder"
