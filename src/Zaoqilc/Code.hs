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
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Applicative
import Numeric.Natural
import Data.Function
import Zaoqilc.Common

type Locale = String
type RawCode a = (a, Char)
type Token a = [RawCode a]
type Line a = (Natural, [Token a])
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
    in map ed x
  where
    ed [(a, '\n')] = [(a, ';')]
    ed x = x

mklines :: [[Token a]] -> StateT [String] (Either (Error a)) [Line a]
mklines [] = return []
mklines (x:xs) =
    do
        s@(sx : ss) <- get
        let (xb, xe) = cs x
        cpr <- lift $ cp xb sx ss (x & concat & unzip & fst)
        let s' = case cpr of {
            (GT, _) -> (xb : s) ;
            (LT, s'') -> s'' ;
            (EQ, _) -> s ;
        }
        put s'
        r <- mklines xs
        return $ (toEnum (length s'), x) : r
  where
    cs :: [Token a] -> (String, [Token a])
    cs a@(x : t) = let y@(h:_) = snd $ unzip x in
        if h `elem` "\t " then (y, t)
                          else ("", a)
    cp :: String -> String -> [String] -> [a] -> Either (Error a) (Ordering, [String])
    cp x s ss a
        | x == s = Right (EQ, undefined)
        | length x > length s = Right (GT, undefined)
        | length x == length s = Left $ Error "缩进错误" a
        | ss == [] = Left $ Error "缩进错误" a
        | otherwise = -- length x < length s
            let s : sx = ss in case cp x s sx a of
                Right (EQ, _) -> Right (LT, ss)
                y@(Right (LT, _)) -> y
                e@(Left _) -> e
                _ -> Left $ Error "缩进错误" a

makeLines :: [Token a] -> Either (Error a) [Line a]
makeLines x = case runStateT (ct x & mklines) [""] of
    (Left e) -> Left e
    (Right (x, _)) -> Right x
  where
    c [(_, ';')] = True
    c _ = False
    ct [] = []
    ct xs = let (y, ys) = break c xs in case ys of
        [] -> [y]
        (z : zs) -> (y ++ [z]) : ct zs
