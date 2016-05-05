module Set1 ( mkSeed
            , fiveRands
            , randEven
            , randOdd
            , randTen
            , randPair
            , randPair'
            , generalPair
            , repRandom
            , randString3
            , randLetter) where

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

type Gen a = Seed -> (a, Seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g seed = (f $ fst r, snd r)
  where r = g seed

genRand :: Gen Integer
genRand = rand

genRandLetter :: Gen Char
genRandLetter = randLetter

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (2*) genRand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA ((1+) . (2*)) genRand

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (10*) genRand

randPair   :: Gen (Char, Integer)
randPair s = ((fst rl, fst ri), snd ri)
  where
    rl = randLetter s
    ri = genRand $ snd rl

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair g1 g2 s = ((x, y), s2)
    where (x, s1) = g1 s
          (y, s2) = g2 s1

randPair' :: Gen (Char, Integer)
randPair' = generalPair randLetter genRand

repRandom :: [Gen a] -> Gen [a]
repRandom [] s     = ([], s)
repRandom (g:gs) s = (x:xs, ss)
    where (x, s1)  = g s
          (xs, ss) = repRandom gs s1

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f s  = f x s1
  where (x, s1) = g s

mkGen :: a -> Gen a
mkGen a s = (a, s)
