-- https://adventofcode.com/2017/day/2
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}

module Aoc2017Day2 where

import Control.Applicative
import Control.Monad

samples :: [[Int]]
samples = [ [5, 9, 2, 8]
        , [9, 4, 7, 3]
        , [3, 8, 6, 5]
        ]

solveLine1 :: [Int] -> Int
solveLine1 xs = maximum xs - minimum xs

solveLine2 :: [Int] -> Int
solveLine2 xs = head $ do
    (x, y) <- (,) <$> xs <*> xs
    guard (x /= y)
    if | ((x `mod` y) == 0) -> return $ x `div` y
       | ((y `mod` x) == 0) -> return $ y `div` x
       | otherwise -> empty

main :: IO ()
main = do
    print . sum . fmap solveLine1 $ samples
    print . sum . fmap solveLine2 $ samples
