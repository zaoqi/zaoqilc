{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
class App f p r where
    app :: f -> p -> r
instance App ([a] -> b) a ([a] -> b) where
    app f p = \a -> f (p:a)
instance App (a -> b) a b where
    app f p = f p

class Call f r where
    call :: f -> r
instance Call ([a] -> b) b where
    call f = f []
instance Call a a where
    call = id