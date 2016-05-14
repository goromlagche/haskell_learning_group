module Set2Test where

import Test.Hspec
import Test.QuickCheck

import Set2

main :: IO()
main = hspec $ do
  describe "headMay" $ do
    it "passess the provided specs" $ do
      queryGreek greekDataA "alpha" `shouldBe` MyJust 2.0
      queryGreek greekDataA "beta" `shouldBe` MyNothing
      queryGreek greekDataA "gamma" `shouldBe` MyJust 3.3333333333333335
      queryGreek greekDataA "delta" `shouldBe` MyNothing
      queryGreek greekDataA "zeta" `shouldBe` MyNothing
      queryGreek greekDataB "rho" `shouldBe` MyNothing
      queryGreek greekDataB "phi" `shouldBe` MyJust 0.24528301886792453
      queryGreek greekDataB "chi" `shouldBe` MyJust 9.095238095238095
      queryGreek greekDataB "psi" `shouldBe` MyNothing
      queryGreek greekDataB "omega" `shouldBe` MyJust 24.0
