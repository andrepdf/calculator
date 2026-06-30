module Parser
    ( Parser(..)
    , expression )
where

import Data.Char           ( isSpace, isDigit )
import Control.Applicative ( Alternative(..), asum )
import Syntax              ( Expr(..) )

--- Parser Type ---

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f px = Parser $ \s0 -> do
        (x, s1) <- parse px s0
        return (f x, s1)

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser $ \s0 -> Just (x, s0)
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = Parser $ \s0 -> do
        (f, s1) <- parse pf s0
        (x, s2) <- parse px s1
        return (f x, s2)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= f = Parser $ \s0 -> do
        (x, s1) <- parse px s0
        parse (f x) s1

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser $ \_ -> Nothing
    -- (<|>) :: Parser a -> Parser a -> Parser a
    px <|> py = Parser $ \s0 -> parse px s0 <|> parse py s0

--- Basic Parsers ---

(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) px pxs = (:) <$> px <*> pxs
infixr 5 <:>

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
(<++>) pxs pys = (++) <$> pxs <*> pys
infixr 5 <++>

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s0 -> case s0 of
    []     -> Nothing
    x : xs -> if f x then Just (x, xs) else Nothing

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 px pf = px >>= fold
    where
        fold x = asum
            [ pf >>= \f -> px >>= \y -> fold (f x y)
            , return x ]

unit :: Parser Char
unit = satisfy (\_ -> True)

void :: Parser ()
void = Parser $ \s0 -> case s0 of
    [] -> Just ((), s0)
    _  -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = many space

digit :: Parser Char
digit = satisfy isDigit

whole :: Parser String
whole = some digit

integer :: Parser String
integer = char '-' <:> whole <|> whole

real :: Parser String
real = integer <++> (char '.' <:> whole) <|> integer

--- Expression Parsers ---

constant :: Parser Expr
constant = Con . read <$> real <* spaces

parentheses :: Parser Expr -> Parser Expr
parentheses px = char '(' *> spaces *> px <* char ')' <* spaces

expr1 :: Parser Expr
expr1 = asum
    [ constant
    , Neg <$> (char '-' *> spaces *> parentheses expr3)
    , parentheses expr3 ]

expr2 :: Parser Expr
expr2 = chainl1 expr1 ops
    where
        ops = asum
            [ Mul <$ (char '*' <* spaces)
            , Div <$ (char '/' <* spaces) ]

expr3 :: Parser Expr
expr3 = chainl1 expr2 ops
    where
        ops = asum
            [ Add <$ (char '+' <* spaces)
            , Sub <$ (char '-' <* spaces) ]

expression :: Parser Expr
expression = spaces *> expr3 <* void
