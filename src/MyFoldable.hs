{-# LANGUAGE InstanceSigs #-}
module MyFoldable where

import Data.Foldable

data MyPair a = MyPair
  { firstElem :: a
  , secondElem :: a
  }

instance Foldable MyPair where
  foldr :: (a -> b -> b) -> b -> MyPair a -> b
  foldr f def (MyPair firstOne secondOne) = f firstOne ((f secondOne) def)
