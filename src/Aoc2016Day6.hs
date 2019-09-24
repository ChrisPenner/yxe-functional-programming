-- https://adventofcode.com/2016/day/6
{-# LANGUAGE QuasiQuotes #-}
module Aoc2016Day6 where

import Text.RawString.QQ
import qualified Data.Map.Monoidal as M
import Data.Foldable
import Data.Function
import Data.Monoid

samples :: [String]
samples = lines [r|eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar|]

handleLine :: String -> [M.MonoidalMap Char (Sum Int)]
handleLine s = go <$> s
  where
    go c = M.singleton c 1

handleLines :: [String] -> [M.MonoidalMap Char (Sum Int)]
handleLines xs = foldl' (zipWith collapse) (repeat mempty) (handleLine <$> xs)
  where
    collapse m n = M.unionWith (+) m n

getLargest :: [M.MonoidalMap Char (Sum Int)] -> String
getLargest ms = fmap go ms
  where
    go :: M.MonoidalMap Char (Sum Int) -> Char
    go m = fst . minimumBy (compare `on` snd) . M.toList $ m

main :: IO ()
main = do
    print . getLargest . handleLines $ samples
