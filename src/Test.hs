-- ghci Test.hs interpreter.c -package QuickCheck
-- quickCheck prop_equalInterpreterOutput

{-# LANGUAGE CApiFFI #-}

module Test where

import Test.QuickCheck       ( Arbitrary(..), oneof,
                               Property, ioProperty, quickCheck )
import Data.Word             ( Word8, Word32 )
import Foreign.Ptr           ( Ptr )
import Foreign.Marshal.Array ( withArray )
import Foreign.Marshal.Utils ( with )

import Syntax      ( Expr(..) )
import Interpreter ( interpret )
import Compiler    ( compile )

foreign import capi "interpreter.h interpret"
    c_interpret :: Ptr Word8 -> Ptr Word32 -> IO Double

instance Arbitrary Expr where
    -- arbitrary :: Gen Expr
    arbitrary = arbitraryExpr 10
        where
            -- arbitraryExpr :: Int -> Gen Expr
            arbitraryExpr 0 = Con <$> arbitrary
            arbitraryExpr i = oneof
                [ Neg <$> arbitraryExpr (i - 1)
                , Add <$> arbitraryExpr (i - 1) <*> arbitraryExpr (i - 1)
                , Sub <$> arbitraryExpr (i - 1) <*> arbitraryExpr (i - 1)
                , Mul <$> arbitraryExpr (i - 1) <*> arbitraryExpr (i - 1)
                , Div <$> arbitraryExpr (i - 1) <*> arbitraryExpr (i - 1) ]

interpretBytecode :: [Word8] -> IO (Maybe Double)
interpretBytecode xs =
    withArray xs $ \xptr ->
        with 0 $ \iptr -> do
            res <- c_interpret xptr iptr
            return $ if isNaN res
                then Nothing
                else Just res

prop_equalInterpreterOutput :: Expr -> Property
prop_equalInterpreterOutput expr = ioProperty $ do
    let hsResult = interpret expr
    cResult <- interpretBytecode (compile expr)
    return $ hsResult == cResult
