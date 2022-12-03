module Day3 where

import Data.Char
import Data.List
import Data.List.Split

toNumber :: Char -> Int
toNumber c = let
              i = ord c
             in if i > 90 then i - 96 else i - 64 + 26

parse :: String -> [Int]
parse = map toNumber

part1 :: [[Int]] -> Int
part1 xs = sum $ map go xs
  where go xs = sum $ nub diff
          where n = (length xs) `div` 2
                a = take n xs
                b = drop n xs
                diff = filter (\x -> elem x b ) a

part2 :: [String] -> Int
part2 xs = sum $ map go ys
    where ys = chunksOf 3 xs
          go :: [String] -> Int
          go xs = toNumber $ (head (b !! 0))
                where ys = groupBy (\x y -> x == y) $ sort $ concat (map nub xs)
                      b = filter (\x -> length x == 3) ys

solve :: String -> (String, String)
solve input = (s1, s2)
  where inp = lines input
        numbers = map parse inp 
        s1 = show $ part1 numbers -- 8053
        s2 = show $ part2 inp -- 2425
