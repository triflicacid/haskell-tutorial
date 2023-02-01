import Control.Applicative
import Control.Monad (ap, liftM)

--  An implementation of Rusts `Option' in Haskell
data Option a = None | Some a

instance Functor Option where
  fmap = liftM

instance Applicative Option where
  pure = return
  (<*>) = ap

instance Monad Option where
  None >>= f = None -- Propagate `None` values
  (Some x) >>= f = f x -- Apply `Some` values to the given function

  return = Some -- Construct a `Some` from a raw value