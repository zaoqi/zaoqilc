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
{-# LANGUAGE MultiWayIf #-}
module Zaoqilc.Code (
    Code(Atom, Symbol, Integer, List),
    RawCode(RawCode)
    ) where

data Code a = Atom a String | Symbol a [String] | Integer a Integer | List a [Code a]
data RawCode a = RawCode [(a, Char)]

instance Show (RawCode a) where
    show (RawCode ((_,c):xs)) = c:show (RawCode xs)
    show (RawCode []) = ""

instance (Integral a) => Read (RawCode a) where
    readsPrec _ s = [(RawCode (zip [1..] s), "")]

readCoding _ "" = ("", "")
readCoding end l@(x:xs) = if end x then ("", l)
                                   else let (a, b) = readCoding end xs
                                        in (x:a, b)
readCode _ _ "" = Nothing
readCode begin end (x:xs) = if begin x
                              then let o@(a, b) = readCoding end xs
                                   in if a/="" then Just o
                                               else Nothing
                              else Nothing
readEnd c = c `elem` [' ', '(', ')', '{', '}']
readCodeX begin end ok s = do
                             (a, b) <- readCode begin end s
                             return (ok a, b)

readAtom = readCode (==':') readEnd
