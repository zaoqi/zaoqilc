module Control.Monad.Trans.M2 where
newtype M2 f g a = M2 {runM2 :: f (g a)}
instance (Functor f,Functor g) => Functor (M2 f g) where
	fmap f (M2 x) = M2 $ fmap (fmap f) x