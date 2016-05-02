module Set1Test where

import MCPrelude
import Set1
import Test.Hspec

main :: IO()
main = hspec $ do
  describe "fiveRands" $ do
    it "passes the the spec mentioned in the exercise" $ do
      product(fiveRands) `shouldBe` 8681089573064486461641871805074254223660
  describe "randEven randOdd randTen" $ do
    it "passes the the spec mentioned in the exercise" $ do
      product[(fst $ randEven $ mkSeed 1), (fst $ randOdd $ mkSeed 1), (fst $ randTen $ mkSeed 1)]
      `shouldBe` 189908109902700
