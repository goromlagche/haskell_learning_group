module Set1 where

import Data.Monoid
import MCPrelude

fiveRands :: [Integer]
fiveRands = getList $ mkRand [rand $ mkSeed 1] 5

mkRand :: [(Integer, Seed)] -> Int -> [(Integer, Seed)]
mkRand n@((_, s) : _) x = if length(n) == x
                          then n
                          else mkRand ([(rand s)] <> n) x

getList :: [(x,y)] -> [x]
getList [] = []
getList ((x,_):xs) = [x] <> getList xs


-- Tests
import Test.Hspec

main :: IO()
main = hspec $ do
  describe "fiveRands" $ do
    it "passes the the spec mentioned in the exercise" $ do
      product(fiveRands) `shouldBe` 8681089573064486461641871805074254223660
