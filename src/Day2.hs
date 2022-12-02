module Day2 where

data Hand = Rock
          | Paper
          | Scissors
            deriving(Enum, Eq)

data Result = Loss
            | Draw
            | Win
              deriving(Enum)

game :: (Hand, Hand) -> Result
game (Rock, Paper) = Win
game (Paper, Scissors) = Win
game (Scissors, Rock) = Win
game (x, y)
    | x == y = Draw
    | otherwise = Loss

parse1 :: String -> (Hand, Hand)
parse1 xs = (l x, r y)
    where
        x = head xs
        y = last xs
        l 'A' = Rock
        l 'B' = Paper
        l 'C' = Scissors
        r 'X' = Rock
        r 'Y' = Paper
        r 'Z' = Scissors

scoreHand :: Hand -> Int
scoreHand x = 1 + fromEnum x

scoreGame :: Result -> Int
scoreGame x = 3 * fromEnum x

score1 :: (Hand, Hand) -> Int
score1 all@(_,y) = (scoreHand y) + (scoreGame (game all))

part1 :: [String] -> Int
part1 xs = sum $ map (score1 . parse1) xs

parse2 :: String -> (Hand, Result)
parse2 xs = (l x, r y)
    where
        x = head xs
        y = last xs
        l 'A' = Rock
        l 'B' = Paper
        l 'C' = Scissors
        r 'X' = Loss
        r 'Y' = Draw
        r 'Z' = Win

predicatedHand :: (Hand, Result) -> Hand
predicatedHand (h, r) = toEnum $ ((fromEnum h) + (fromEnum r) + 2) `mod` 3

score2 :: (Hand, Result) -> Int
score2 all@(_,y) = (scoreGame y) + (scoreHand (predicatedHand all))

part2 :: [String] -> Int
part2 xs = sum $ map (score2 . parse2) xs

solve :: String -> (String, String)
solve input = (s1, s2)
  where xs = lines input
        s1 = show $ part1 xs -- 15422
        s2 = show $ part2 xs -- 15442
