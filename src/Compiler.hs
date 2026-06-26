module Compiler where

import Data.Word ( Word8 )
import Syntax    ( Expr(..) )

compile :: Expr -> [Word8]
compile (Con x)   = [0x00, fromIntegral x]
compile (Add x y) = [0x01] ++ compile x ++ compile y
compile (Sub x y) = [0x02] ++ compile x ++ compile y
compile (Mul x y) = [0x03] ++ compile x ++ compile y
