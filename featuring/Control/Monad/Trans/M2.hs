module Control.Monad.Trans.M2 where
import Data.Function
import Control.Monad
import Control.Monad.Trans
import Data.Foldable

newtype M2 f g a = M2 {runM2 :: f (g a)}

instance (Functor f,Functor g) => Functor (M2 f g) where
    fmap f (M2 x) = M2 $ fmap (fmap f) x

instance (Applicative f,Applicative g) => Applicative (M2 f g) where
    pure = M2 . pure . pure
    M2 f <*> M2 x = M2 $ (f & fmap (<*>) & (<*>)) x

instance (Monad f,Monad g,Traversable g) => Monad (M2 f g) where
    return = M2 . return . return
    x >>= f = fmap f x & runM2 & fmap (fmap runM2) & fmap sequence & join & fmap join & M2

instance Applicative f => MonadTrans (M2 f) where
    lift = M2 . pure

instance (Traversable f,Traversable g) => Traversable (M2 f g) where
    sequenceA (M2 x) = x & fmap sequenceA & sequenceA & fmap M2

instance (Foldable f,Foldable g) => Foldable (M2 f g) where
    foldr f x (M2 xs) = xs & toList & map toList & join & foldr f x
