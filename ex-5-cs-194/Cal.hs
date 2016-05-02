module Cal where

import ExprT
import Parser
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (ExprT.Lit a)   = a
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s  = fmap eval $ parseExp ExprT.Lit ExprT.Add ExprT.Mul s

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit                       = MinMax
  add (MinMax a) (MinMax b) = lit $ max a b
  mul (MinMax a) (MinMax b) = lit $ min a b

instance Expr Mod7 where
  lit n                 = Mod7 $ n `mod` 7
  add (Mod7 a) (Mod7 b) = lit $ a + b
  mul (Mod7 a) (Mod7 b) = lit $ a * b

class HasVars a where
  var :: String -> a

data VarExprT = Var String
              | Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              deriving (Show, Eq)

instance Expr VarExprT where
 lit = Cal.Lit
 add = Cal.Add
 mul = Cal.Mul

instance HasVars VarExprT where
 var = Var
