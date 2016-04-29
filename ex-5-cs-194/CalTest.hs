module CalTest where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid

import ExprT
import Parser
import Cal

main :: IO ()
main = hspec $ do
  describe "Cal" $ do
    describe "eval" $ do
      it "passes the the spec mentioned in the exercise" $ do
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
      it "adds two integers" $ do
        property $ \x y -> eval (Add (Lit x) (Lit y)) == x + y
      it "multiplies two integers" $ do
        property $ \x y -> eval (Mul (Lit x) (Lit y)) == x * y
      it "add and then multiplies" $ do
        property $ \x y z ->
          eval (Mul (Add (Lit x) (Lit y)) (Lit z)) == (x + y) * z

    describe "evalStr" $ do
      it "passes the the spec mentioned in the exercise" $ do
        evalStr "(2+3)*4" `shouldBe` Just 20
      it "does addition" $ do
        property $ \x y -> evalStr (show x <> "+" <> show y) == Just (x + y)
      it "does multiplication" $ do
        property $ \x y -> evalStr (show x <> "*" <> show y) == Just (x * y)
      it "adds and then multiplies" $ do
        property $ \x y z ->
          evalStr ("(" <> show x <> "+" <> show y <> ")" <> "*" <> show z) ==
          Just ((x + y) * z)
