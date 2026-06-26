-- gcc -c interpreter.c -o interpreter.o
-- ghci Test.hs interpreter.o -package QuickCheck
-- quickCheck prop_equalInterpreterOutput

{-# LANGUAGE ForeignFunctionInterface #-}

module Test where

import Data.Word             ( Word8, Word32 )
import Foreign.Ptr           ( Ptr )
import Foreign.Marshal.Array ( newArray )
import Foreign.Marshal.Utils ( new )
import Test.QuickCheck       ( Property, ioProperty, quickCheck, (==>) )

import Syntax
import Compiler
import Interpreter

foreign import ccall "interpret" c_interpret :: Ptr Word8 -> Ptr Word32 -> IO Word8

prop_equalInterpreterOutput :: Expr -> Property
prop_equalInterpreterOutput expr =
    ioProperty $ do
        instrPtr <- newArray (compile expr)
        indexPtr <- new 0
        cResult <- c_interpret instrPtr indexPtr
        let hsResult = interpret expr
        return $ (hsResult <= 255) ==> (cResult == fromIntegral hsResult)
