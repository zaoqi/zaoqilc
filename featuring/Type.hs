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
{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies, PolyKinds, AllowAmbiguousTypes #-}
import Data.Nat

infix 4 ==:
type family (==:) a b where
    a ==: a = True
    _ ==: _ = False

infixl 6 +:
type family (+:) (a::c) (b::d)

type family AddNat (a::Nat) (b::Nat) where
    AddNat Z x = x
    AddNat (S x) y = S (AddNat x y)

type instance x +: y = AddNat x y
