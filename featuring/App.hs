{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Dynamic
import Data.Typeable
class App t where
    app :: t
instance App (([a] -> b) -> a -> ([a] -> b)) where
    app f p = \a -> f (p:a)
instance App ((a -> b) -> (a -> b)) where
    app = id
instance (Typeable b) => App (([Dynamic] -> a) -> b -> ([Dynamic] -> a)) where
    app f p = \a -> f (toDyn p:a)

class Call f r where
    call :: f -> r
instance Call ([a] -> b) b where
    call f = f []
instance Call a a where
    call = id