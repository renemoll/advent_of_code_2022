module Day1 where

import Data.List
import Data.List.Split

parse :: String -> Int
parse x = sum $ map (read :: String -> Int) $ lines x

part1 :: Ord a => [a] -> a
part1 = maximum

sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse . sort

part2 :: Ord a => Num a => [a] -> a
part2 a = sum $ take 3 $ sortDesc a

solve :: String -> (String, String)
solve input = (s1, s2)
  where numbers = map parse $ splitOn "\n\n" input
        s1 = show $ part1 numbers -- 66306
        s2 = show $ part2 numbers -- 195292
