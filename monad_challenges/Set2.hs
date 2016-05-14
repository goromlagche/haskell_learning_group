module Set2 (mkSeed
            , MyMaybe(..)
            , headMay
            , tailMay
            , lookupMay
            , divMay
            , maximumMay
            , minimumMay) where

import MCPrelude

data MyMaybe a = MyNothing | MyJust a deriving (Eq)

instance Show a => Show (MyMaybe a) where
  show MyNothing  = "MyNothing"
  show (MyJust a) = "MyJust " ++ show a

headMay :: [a] -> MyMaybe a
headMay []     = MyNothing
headMay (a:_) = MyJust a

tailMay :: [a] -> MyMaybe [a]
tailMay []     = MyNothing
tailMay (_:as) = MyJust as

lookupMay :: Eq a => a -> [(a, b)] -> MyMaybe b
--  lookup already does this :-/
lookupMay _ [] = MyNothing
lookupMay a ((x,y):xs)
  | a == x    = MyJust y
  | otherwise = lookupMay a xs


divMay :: (Eq a, Fractional a) => a -> a -> MyMaybe a
divMay _ 0   = MyNothing
divMay a1 a2 = MyJust (a1 / a2)

maximumMay :: Ord a => [a] -> MyMaybe a
maximumMay [] = MyNothing
maximumMay n  = MyJust (maximum n)

minimumMay :: Ord a => [a] -> MyMaybe a
minimumMay [] = MyNothing
minimumMay n  = MyJust (minimum n)
