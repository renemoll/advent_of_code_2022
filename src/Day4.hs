module Day4 where

import Data.List.Split

type Fields = [Int]
type Assignment = ([Int], [Int])

toFields :: [String] -> Fields
toFields xs = [(head ints)..(last ints)]
    where ints = map (read :: String -> Int) xs

parse :: String -> Assignment
parse xs = (head zs, last zs)
      where ys = map (splitOn "-") (splitOn "," xs)
            zs = map toFields ys

contained :: Assignment -> Bool
contained xs = p1 || p2
      where p1 = and $ map (\x -> x `elem` (fst xs)) (snd xs)
            p2 = and $ map (\x -> x `elem` (snd xs)) (fst xs)

part1 :: [Assignment] -> Int
part1 xs = length $ filter (== True) (map contained xs)

overlap :: Assignment -> Bool
overlap xs = or $ map (\x -> x `elem` (fst xs)) (snd xs)

part2 :: [Assignment] -> Int
part2 xs = length $ filter (== True) (map overlap xs)

solve :: String -> (String, String)
solve input = (s1, s2)
  where numbers = map parse $ lines input 
        s1 = show $ part1 numbers -- 453
        s2 = show $ part2 numbers -- 919
