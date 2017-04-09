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
    RawCode,
    showRawCode,
    readRawCode,
    unreadRawCode,
    rawCodeGetPos
    ) where

data Code a = Atom a String | Symbol a [String] | Integer a Integer | List a [Code a]
type RawCode a = [(a, Char)]

showRawCode ((_,c):xs) = c:showRawCode xs
showRawCode [] = []
readRawCode = zip [1..]
unreadRawCode = snd . unzip
rawCodeGetPos = fst . head

readCoding _ [] = ([], [])
readCoding end l@(x:xs) = if end x then ([], l)
                                   else let (a, b) = readCoding end xs
                                        in (x:a, b)
readCode _ _ [] = Nothing
readCode begin end l@(x:xs) = if begin x
                              then let o@(a, b) = readCoding end l
                                   in if a/=[] then Just o
                                               else Nothing
                              else Nothing
readEnd (_, c) = c `elem` [' ', '(', ')', '{', '}']
readCodeX begin end ok s = do
                             (a, b) <- readCode begin end s
                             return (ok a, b)

readAtom :: (Eq a) => RawCode a -> Maybe (RawCode a, RawCode a)
readAtom = readCode (\(_,x)->x==':') readEnd
