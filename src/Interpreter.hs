module Interpreter
    ( interpret )
where

import Syntax ( Expr(..) )

interpret :: Expr -> Maybe Double
interpret (Con x) = Just x

interpret (Neg x) = interpret x >>= Just . (0 -)

interpret (Add x y) = do
    x' <- interpret x
    y' <- interpret y
    Just $ x' + y'

interpret (Sub x y) = do
    x' <- interpret x
    y' <- interpret y
    Just $ x' - y'

interpret (Mul x y) = do
    x' <- interpret x
    y' <- interpret y
    Just $ x' * y'

interpret (Div x y) = do
    x' <- interpret x
    y' <- interpret y
    if y' == 0.0
        then Nothing
        else Just $ x' / y'
