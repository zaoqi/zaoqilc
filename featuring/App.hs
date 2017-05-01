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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Dynamic

newtype LP a = LP a
newtype DP a = DP a
newtype D a = D a

withLP = LP
withDP = DP

class App t where
    app :: t

instance (a ~ c, d ~ (LP ([a] -> b))) => App ((LP ([a] -> b)) -> c -> d) where
    app (LP f) p = LP (\a -> f (p:a))

instance (a ~ c, (D b) ~ d) => App ((a -> b) -> c -> d) where
    app f p = D $ f p

instance (Typeable b, c ~ (DP ([Dynamic] -> a))) => App ((DP ([Dynamic] -> a)) -> b -> c) where
    app (DP f) p = DP (\a -> f (toDyn p:a))

instance App (f -> a -> b) => App ((D f) -> a -> b) where
    app (D f) p = app f p

class Call t where
    call :: t

instance a ~ b => Call ((D a) -> b) where
    call (D x) = x

instance b ~ c => Call ((LP ([a] -> b)) -> c) where
    call (LP f) = f []

instance b ~ c => Call ((DP ([Dynamic] -> b)) -> c) where
    call (DP f) = f []
