module Set2Test where

import Test.Hspec
import Test.QuickCheck

import Set2

-- getting some error which I can't debug :(
-- main :: IO()
-- main = hspec $ do
--   describe "headMay" $ do
--     describe "provides safe head" $ do
--       it "returns Nothing for empty list" $
--         headMay([]) `shouldBe` MyNothing
--       it "returns Head for list" $
--         property $ \[x] -> headMay [x] == MyJust (head [x])
