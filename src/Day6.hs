module Day6 where

import Data.List
import Data.Maybe

view :: Int -> String -> [Bool]
view _ [] = []
view n xs = b:(view n (drop 1 xs))
    where a = take n xs
          b = length (nub a) == n

part1 :: String -> Int
part1 xs = n + fromJust index
    where n = 4
          index = findIndex (== True) (view n xs)

part2 :: String -> Int
part2 xs = n + fromJust index
    where n = 14
          index = findIndex (== True) (view n xs)

solve :: String -> (String, String)
solve input = (s1, s2)
  where s1 = show $ part1 input -- 1757
        s2 = show $ part2 input -- 2950
