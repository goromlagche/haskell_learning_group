module Cal where

import ExprT
import Test.QuickCheck

eval :: ExprT -> Integer
eval (Lit a)   = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b