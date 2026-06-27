module Parser where

import Data.Char
import Syntax

type Parser a = String -> Maybe (a, String)

unit :: Parser Char
unit ""       = Nothing
unit (x : xs) = Just (x, xs)

filterP :: Parser Char -> (Char -> Bool) -> Parser Char
filterP p f xs = p xs >>= \(y, ys) -> if f y then Just (y, ys) else Nothing

lit :: Char -> Parser Char
lit c = filterP unit (== c)

digit :: Parser Char
digit = filterP unit isDigit

many :: Parser a -> Parser [a]
many p xs = p xs >>= \(y, ys) ->
    case many p ys of
        Nothing      -> Just ([y], ys)
        Just (z, zs) -> Just (y : z, zs)

integer :: Parser Int
integer xs = many digit xs >>= \(y, ys) -> Just (read y, ys)

--- Expression Parser ---

constant :: Parser Expr
constant xs =
    case lit '-' xs of
        Nothing      -> integer xs >>= \(y,ys) -> Just (Con y, ys)
        Just (_, ys) -> integer ys >>= \(z,zs) -> Just (Con (-z), zs)

operator :: Parser (Expr -> Expr -> Expr)
operator ('+' : xs) = Just (Add, xs)
operator ('-' : xs) = Just (Sub, xs)
operator ('*' : xs) = Just (Mul, xs)
operator _          = Nothing

binary :: Parser Expr
binary xs = do
    (a, ys) <- constant xs
    (op, zs) <- operator ys
    (b, ws) <- constant zs
    Just (a `op` b, ws)

expression :: Parser Expr
expression xs =
    case binary xs of
        Nothing      -> constant xs
        Just (e, ys) -> Just (e, ys)
