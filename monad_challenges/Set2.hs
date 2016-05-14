module Set2 (mkSeed) where

import MCPrelude

data MyMaybe a = MyNothing | MyJust a

instance Show a => Show (MyMaybe a) where
  show MyNothing = "MyNothing"
  show (MyJust a) = "MyJust " ++ show a
