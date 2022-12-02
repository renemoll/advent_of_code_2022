module Day2 where

parse :: String -> (Char, Char)
parse input = (a, b)
    where [x, y] = words input
          a = head x
          b = head y

score :: Char -> Int
score x = case x of 'X' -> 1
                    'Y' -> 2
                    'Z' -> 3

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

offset :: Char -> Int
offset x = case x of 'A' -> 0
                     'B' -> 1
                     'C' -> 2

scoreGame :: Char -> Char -> Int
scoreGame a b = hand + game
    where hand = score b
          game = (rotate (3 - (offset a)) [3, 6, 0]) !! (hand - 1)

part1 :: [(Char, Char)] -> Int
part1 x = sum $ map work x
    where work x = scoreGame (fst x) (snd x)

scoreGame2 :: Char -> Char -> Int
scoreGame2 a b = hand + game
    where hand = (rotate (offset a) [3, 1, 2]) !! ((score b) - 1)
          game = [0, 3, 6] !! ((score b) - 1)


part2 :: [(Char, Char)] -> Int
part2 x = sum $ map work x
    where work x = scoreGame2 (fst x) (snd x)

solve :: String -> (String, String)
solve input = (s1, s2)
  where numbers = map parse $ lines input
        s1 = show $ part1 numbers -- 15422
        s2 = show $ part2 numbers -- show $ part2 numbers -- ?
