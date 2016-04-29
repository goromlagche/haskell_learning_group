module Cal where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit a)   = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s  = fmap eval $ parseExp Lit Add Mul s

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul
