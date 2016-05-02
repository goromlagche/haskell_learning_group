module Set1 where

import Data.Monoid
import qualified MCPrelude  as M

fiveRands :: [Integer]
fiveRands = getList $ mkRand [M.rand $ M.mkSeed 1] 5

mkRand :: [(Integer, M.Seed)] -> Int -> [(Integer, M.Seed)]
mkRand [] _             = []
mkRand n@((_, s) : _) x = if length n == x
                          then n
                          else mkRand ([M.rand s] <> n) x


getList :: [(x,y)] -> [x]
getList [] = []
getList ((x,_):xs) = getList xs <> [x]

randLetter :: M.Seed -> (Char, M.Seed)
randLetter s = convertLetter $ M.rand s
  where
    convertLetter (x, y) = (M.toLetter x, y)

mkRandLetter :: [(Char, M.Seed)] -> Int -> [(Char, M.Seed)]
mkRandLetter [] _              = []
mkRandLetter n@((_, s) : _) x = if length n == x
                          then n
                          else mkRandLetter ([randLetter s] <> n) x

randString3 :: String
randString3 = getList $ mkRandLetter [randLetter $ M.mkSeed 1] 3

type Gen a = M.Seed -> (a, M.Seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g seed = (f $ fst r, snd r)
  where r = g seed

genRand :: Gen Integer
genRand = M.rand

genRandLetter :: Gen Char
genRandLetter = randLetter

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (2*) genRand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA ((1+) . (2*)) genRand

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (10*) genRand
