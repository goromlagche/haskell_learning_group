module Set2Test where

import Test.Hspec
import Test.QuickCheck

import Set2

main :: IO()
main = hspec $ do
  describe "queryGreek" $ do
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

  describe "queryGreek2" $ do
    it "passess the provided specs" $ do
      queryGreek2 greekDataA "alpha" `shouldBe` MyJust 2.0
      queryGreek2 greekDataA "beta" `shouldBe` MyNothing
      queryGreek2 greekDataA "gamma" `shouldBe` MyJust 3.3333333333333335
      queryGreek2 greekDataA "delta" `shouldBe` MyNothing
      queryGreek2 greekDataA "zeta" `shouldBe` MyNothing
      queryGreek2 greekDataB "rho" `shouldBe` MyNothing
      queryGreek2 greekDataB "phi" `shouldBe` MyJust 0.24528301886792453
      queryGreek2 greekDataB "chi" `shouldBe` MyJust 9.095238095238095
      queryGreek2 greekDataB "psi" `shouldBe` MyNothing
      queryGreek2 greekDataB "omega" `shouldBe` MyJust 24.0
