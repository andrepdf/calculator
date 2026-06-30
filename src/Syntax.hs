module Syntax
    ( Expr(..) )
where

data Expr
    = Con Double
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Eq, Show)
