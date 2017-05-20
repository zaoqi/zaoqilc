--Zaoqilc
--Copyright (C) 2017  Zaoqi

--This program is free software: you can redistribute it and/or modify
--it under the terms of the GNU Affero General Public License as published
--by the Free Software Foundation, either version 3 of the License, or
--(at your option) any later version.

--This program is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU Affero General Public License for more details.

--You should have received a copy of the GNU Affero General Public License
--along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.HList (
    HList(..),
    getByType,
    curry2HList,
    hList2Curry,
    (++?),
    hnull
    ) where
import Data.Typeable

infixr 6 :?
data HList :: [*] -> * where
    HNil  :: HList '[]
    (:?) :: x -> HList xs -> HList (x ': xs)

type family HasType a (xs :: [*]) where
    HasType a '[]       = False
    HasType a (a ': xs) = True
    HasType a (b ': xs) = HasType a xs

class GetByType a xs where
    getByType :: HList xs -> a
instance {-# OVERLAPPING #-} HasType a xs ~ False => GetByType a (a ': xs) where
    getByType (x :? _) = x
instance GetByType a xs => GetByType a (b ': xs) where
    getByType (_ :? xs) = getByType xs

class Curry2HList f l r where
    curry2HList :: f -> HList l -> r
instance (c ~ a, d ~ b) => Curry2HList (a -> b) '[c] d where
    curry2HList f (x :? HNil) = f x
instance (x ~ a, Curry2HList b (y ': ys) d) => Curry2HList (a -> b) (x ': (y ': ys)) d where
    curry2HList f (x :? xs) = curry2HList (f x) xs

class HList2Curry h r f where
    hList2Curry :: (HList h -> r) -> f
instance c ~ (a -> b) => HList2Curry '[a] b c where
    hList2Curry f = \x -> f (x :? HNil)
instance (HList2Curry (b ': c) d f, e ~ (a -> f)) => HList2Curry (a ': (b ': c)) d e where
    hList2Curry f = \x -> hList2Curry $ \h -> f (x :? h)

hnull HNil = True
hnull _ = False

class Happ a b c where
    (++?) :: a -> b -> c
instance (Happ (HList b) (HList c) (HList e), d ~ (HList (a ': e))) => Happ (HList (a ': b)) (HList c) d where
    (x :? xs) ++? ys = x :? (xs ++? ys)
instance (b ~ HList a) => Happ (HList '[]) (HList a) b where
    HNil ++? x = x

