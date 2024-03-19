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
  pure = Reader <<< const

-- f        :: a -> Reader r b
-- fx       :: r -> a
-- fx r     :: a
-- f $ fx r :: Reader r b
-- Reader \r -> f $ fx r :: Reader r (Reader r b)
-- runReader (f $ fx r) :: r -> b
-- runRedaer (f $ fx r) r :: b
instance bindReader :: Bind (Reader r) where
  --bind (Reader fx) f = join $ Reader \r -> f $ fx r
  bind (Reader fx) f = Reader \r -> runReader (f $ fx r) r

instance monadReader :: Monad (Reader r)

{-- HELPER --}
runReader :: forall a r. Reader r a -> r -> a
runReader (Reader r) = r

test :: Effect Unit
test = do
  log $ show $ "placeholder"
