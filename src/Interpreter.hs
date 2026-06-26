module Interpreter where

import Syntax ( Expr(..) )

interpret :: Expr -> Int
interpret (Con x)   = x
interpret (Add x y) = interpret x + interpret y
interpret (Sub x y) = interpret x - interpret y
interpret (Mul x y) = interpret x * interpret y
