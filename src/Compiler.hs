module Compiler
    ( compile )
where

import Data.Word ( Word8 )
import Data.Bits ( shiftR )
import GHC.Float ( castDoubleToWord64 )
import Syntax    ( Expr(..) )

doubleToBytes :: Double -> [Word8]
doubleToBytes x =
    let w = castDoubleToWord64 x
    in [ fromIntegral (w `shiftR` (i * 8)) | i <- [0..7] ]

compile :: Expr -> [Word8]
compile (Con x)   = 0x00 : doubleToBytes x
compile (Neg x)   = 0x01 : compile x
compile (Add x y) = 0x02 : compile x ++ compile y
compile (Sub x y) = 0x03 : compile x ++ compile y
compile (Mul x y) = 0x04 : compile x ++ compile y
compile (Div x y) = 0x05 : compile x ++ compile y
