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
module Data.Nat where
import Control.Exception

data Nat = Z | S Nat deriving (Eq, Ord)

instance Enum Nat where
    succ = S
    pred (S x) = x
    pred _ = throw Underflow
    toEnum 0 = Z
    toEnum x | x < 0     = throw Underflow
             | otherwise = S . toEnum $ pred x
    fromEnum Z = 0
    fromEnum (S x) = succ $ fromEnum x
    enumFrom x = x : enumFrom (S x)

instance Num Nat where
    Z + x = x
    (S x) + y = S $ x + y
    Z * _ = Z
    (S x) * y = y + (x * y)
    abs = id
    signum Z = Z
    signum _ = S Z
    fromInteger 0 = Z
    fromInteger x | x < 0     = throw Underflow
                  | otherwise = S . fromInteger $ pred x
    x - Z = x
    (S x) - (S y) = x - y
    _ - _ = throw Underflow
    negate Z = Z
    negate _ = throw Underflow

instance Real Nat where
    toRational = toRational . toInteger

instance Integral Nat where
    quot x y = fromInteger $ quot (toInteger x) (toInteger y)
    rem x y = fromInteger $ rem (toInteger x) (toInteger y)
    div x y = fromInteger $ div (toInteger x) (toInteger y)
    mod x y = fromInteger $ mod (toInteger x) (toInteger y)
    quotRem x y = let (x, y) = quotRem (toInteger x) (toInteger y) in (fromInteger x, fromInteger y)
    divMod x y = let (x, y) = divMod (toInteger x) (toInteger y) in (fromInteger x, fromInteger y)
    toInteger Z = 0
    toInteger (S x) = succ $ toInteger x
