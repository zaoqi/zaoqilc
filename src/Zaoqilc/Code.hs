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
module Zaoqilc.Code where
import Control.Monad
import Data.Ratio

type RawCode a = (a, Char)
type Token a = [RawCode a]
data Code a = CodeAtom [a] [String] |
              CodeSymbol [a] [String] |
			  CodeNumber [a] (Ratio Integer) |
			  CodeChar [a] Char |
			  CodeList [a] [Code a]

notSymbol :: RawCode a -> Bool
notSymbol (_, c) = c `elem` "(){}'`\t\r\n "