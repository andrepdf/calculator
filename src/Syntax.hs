module Syntax where

import Test.QuickCheck ( Arbitrary(..), oneof )

data Expr
    = Con Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    deriving (Show)

instance Arbitrary Expr where
    -- arbitrary :: Gen Expr
    arbitrary = arbitraryExpr 10

-- arbitraryExpr :: Int -> Gen Expr
arbitraryExpr 0 = Con <$> arbitrary
arbitraryExpr i = oneof
    [ Con <$> arbitrary
    , Add <$> arbitraryExpr (i - 1) <*> arbitraryExpr (i - 1)
    , Sub <$> arbitraryExpr (i - 1) <*> arbitraryExpr (i - 1)
    , Mul <$> arbitraryExpr (i - 1) <*> arbitraryExpr (i - 1) ]

-- sample $ arbitraryExpr 2
