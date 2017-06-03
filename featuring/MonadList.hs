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
module Zaoqil.Monad.List where

instance Monad [] where
    xs >>= f = j 1 $ map f xs
      where
        p _ [] = ([], [])
        p 0 xs = ([], xs)
        p x ((y:ys):zs) = let (a, b) = p (pred x) zs in (y:a, ys:b)
        j _ [] = []
        j x xs = let (a, b) = p x xs in a ++ j (succ x) b

instance Applicative [] where
    pure x = [x]
    fs <*> xs = do
        f <- fs
        x <- xs
        return $ f x
