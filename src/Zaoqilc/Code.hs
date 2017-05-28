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
{-# LANGUAGE AllowAmbiguousTypes #-}
module Zaoqilc.Code where
import Control.Monad
import Data.Ratio
import Control.Monad.Trans.State.Lazy
import Control.Applicative

type Locale = String
type RawCode a = (a, Char)
type Token a = [RawCode a]
data Code a = CodeAtom [a] [String] |
              CodeSymbol [a] [String] |
              CodeNumber [a] (Ratio Integer) |
              CodeChar [a] Char |
              CodeText [a] [(Locale, String)] | --给人类看的文本，普通字符串用字符的列表来表示
              CodeList [a] [Code a]

notSymbol :: RawCode a -> Bool
notSymbol (_, c) = c `elem` "(){}'`\t\r\n "

nextRawCode :: Monad b => StateT [RawCode a] b (RawCode a)
nextRawCode = do
    x : xs <- get
    put xs
    return x

rawCodeNotSymbol :: MonadPlus b => StateT [RawCode a] b (Token a)
rawCodeNotSymbol = do
    c@(_, x) <- nextRawCode
    guard $ x `elem` "(){}'`[];\n"
    return [c]

rawCodeSpace :: MonadPlus b => StateT [RawCode a] b [RawCode a]
rawCodeSpace = do
    c@(_, x) <- nextRawCode
    guard $ x `elem` "\t "
    (<|> return [c]) $ do
        xs <- rawCodeSpace
        return (c:xs)

rawCodeString :: MonadPlus b => StateT [RawCode a] b [RawCode a]
rawCodeString =
    do
        q@(_, x) <- nextRawCode
        guard $ x `elem` "\"“"
        r <- doing
        return (q:r)
  where
    t = do
        (_, '^') <- nextRawCode
        (a, x) <- nextRawCode
        return . (,) a $ case x of ';' -> '\n'
                                   '>' -> '\t'
                                   '<' -> '\r'
                                   '"' -> '"'
                                   '“' -> '“'
                                   '”' -> '”'
                                   '^' -> '^'
    e = do
        q@(_, x) <- nextRawCode
        guard $ x `elem` "\"”"
        return [q]
    doing = do
        x <- (t <|>) $ do
            x@(_, c) <- nextRawCode
            guard $ c `notElem` "\"“”^"
            return x
        xs <- doing <|> e
        return (x:xs)

rawCodeSymbol :: MonadPlus b => StateT [RawCode a] b [RawCode a]
rawCodeSymbol = do
    c@(_, x) <- nextRawCode
    guard $ x `notElem` "(){}'`[];\n\t\r\"“”^ "
    (<|> return [c]) $ do
        xs <- rawCodeSymbol
        return (c:xs)

rawCode2Token :: [RawCode a] -> [Token a]
rawCode2Token xs =
    let
        Just (x, []) = flip runStateT (filter (\(_,x)->x/='\r') xs) $
            let
                self = do
                    x <- rawCodeSpace <|> rawCodeString <|> rawCodeNotSymbol <|> rawCodeSymbol
                    (<|> return [x]) $ do
                        xs <- self
                        return (x:xs)
            in self
    in x
