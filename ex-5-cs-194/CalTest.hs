module CalTest where
import ExprT
import Cal
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    it "passes the the spec mentioned in the exercise" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
    it "adds two integers" $ do
      property $ \x y -> eval (Add (Lit x) (Lit y)) == x + y
    it "multiplies two integers" $ do
      property $ \x y -> eval (Mul (Lit x) (Lit y)) == x * y
    it "add and then multiplies" $ do
      property $ \x y z -> eval (Mul (Add (Lit x) (Lit y)) (Lit z)) == (x + y) * z
