module Reader.Monadic01 where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

newtype Reader r a
  = Reader (r -> a)

instance functorReader :: Functor (Reader r) where
  map f (Reader g) = Reader \r -> f $ g r

instance applyReader :: Apply (Reader r) where
  apply (Reader ff) (Reader fx) = Reader \r -> ff r $ fx r

instance applicativeReader :: Applicative (Reader r) where
  --pure x = Reader \_ -> x
  pure = Reader <<< const

--                      f           ::       a -> Reader r b
--                          fx      :: (r -> a)
--                          fx r    ::       a
--                      f $ fx r    ::            Reader r b
-- Reader \r ->         f $ fx r    :: Reader r  (Reader r b) -- join but we want something leaner
--                                     ^
--                                  this is where we can use `join`
--                                  however we desire something leaner
--------------- unwrap (f $ fx r)   ::                r -> b
--------------- unwrap (f $ fx r) r ::                     b
-- Reader \r -> unwrap (f $ fx r) r :: Reader r            b
instance bindReader :: Bind (Reader r) where
  --bind (Reader fx) f = join $ Reader \r -> f $ fx r
  --bind (Reader fx) f = Reader \r -> runReader (f $ fx r) r
  bind (Reader fx) f = Reader \r -> (unwrap $ transform r) r
    where
      unwrap (Reader c) = c
      transform         = \r -> f $ fx r

runReader :: forall a r. Reader r a -> r -> a
runReader (Reader f) = f

instance monadReader :: Monad (Reader r)

test :: Effect Unit
test = do
  log $ show $ "placeholder"
