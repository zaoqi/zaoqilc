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
module Control.Monad.Trans.T where
import Data.Function

newtype T g f a = T (f (g a)) deriving (Show)

runT (T x) = x

instance (Functor f, Functor g) => Functor (T g f) where
	fmap f (T x) = T $ fmap (fmap f) x

instance (Applicative f, Applicative g) => Applicative (T g f) where
	pure = T . pure . pure
	T f <*> T x = f & fmap (<*>) & (<*>) & ($ x) & T

instance (Monad f, Monad g, Traversable g) => Monad (T g f) where
	T a >>= f = T $ do
		a' <- a
		r <- sequence $ fmap f a'
		return $ join r