module Set1 where

import Data.Monoid
import MCPrelude

fiveRands :: [Integer]
fiveRands = getList $ mkRand [rand $ mkSeed 1] 5

mkRand :: [(Integer, Seed)] -> Int -> [(Integer, Seed)]
mkRand [] _             = []
mkRand n@((_, s) : _) x = if length n == x
                          then n
                          else mkRand ([rand s] <> n) x


getList :: [(x,y)] -> [x]
getList [] = []
getList ((x,_):xs) = getList xs <> [x]

randLetter :: Seed -> (Char, Seed)
randLetter s = convertLetter $ rand s
  where
    convertLetter (x, y) = (toLetter x, y)

mkRandLetter :: [(Char, Seed)] -> Int -> [(Char, Seed)]
mkRandLetter [] _              = []
mkRandLetter n@((_, s) : _) x = if length n == x
                          then n
                          else mkRandLetter ([randLetter s] <> n) x

randString3 :: String
randString3 = getList $ mkRandLetter [randLetter $ mkSeed 1] 3

-- -- Tests
-- import Test.Hspec

-- main :: IO()
-- main = hspec $ do
--   describe "fiveRands" $ do
--     it "passes the the spec mentioned in the exercise" $ do
--       product(fiveRands) `shouldBe` 8681089573064486461641871805074254223660
