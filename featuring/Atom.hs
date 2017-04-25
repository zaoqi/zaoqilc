{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
data Atom :: [Char] -> * where
    Atom :: Atom a