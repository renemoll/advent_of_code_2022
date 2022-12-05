module Day5 where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map

type State = Map.Map Int String
type Move = (Int, Int, Int)

parseState :: [String] -> Map.Map Int String 
parseState xs = Map.fromListWith (++) $ (keyValue . extract . transpose) xs
    where extract :: [String] -> [String]
          extract [] = []
          extract xs = line : (extract remainder)
                where remainder = drop 4 xs
                      line = (take 4 xs) !! 1
          keyValue :: [String] -> [(Int,String)]
          keyValue = map go
            where go xs = (number, filter (not . isSpace) containers)
                     where number = (digitToInt . last) xs
                           containers = init xs

parseMove :: String -> (Int, Int, Int)
parseMove xs = (n, src, dst)
    where ys = words xs
          n = (read :: String -> Int) (ys !! 1)
          src = (read :: String -> Int) (ys !! 3)
          dst = (read :: String -> Int) (ys !! 5)

parse :: String -> (State, [Move])
parse xs = (state, moves)
    where [ss,ms] = splitOn "\n\n" xs
          state = parseState $ lines ss
          moves = map parseMove $ lines ms

applyMove :: State -> Move -> State
applyMove s (n,src,dst) = Map.insert dst new (Map.insert src (drop n l) s)
    where l = s Map.! src
          new = (reverse (take n l)) ++ (s Map.! dst)

applyMoves :: (State -> Move -> State) -> State -> [Move] -> String
applyMoves f state moves = Map.foldl (\acc v -> acc ++ [head v]) [] newState
    where newState = foldl (\state m -> f state m) state moves

part1 :: State -> [Move] -> String
part1 = applyMoves applyMove

applyMove2 :: State -> Move -> State
applyMove2 s (n,src,dst) = Map.insert dst new (Map.insert src (drop n l) s)
    where l = s Map.! src
          new = (take n l) ++ (s Map.! dst)

part2 :: State -> [Move] -> String
part2 = applyMoves applyMove2

solve :: String -> (String, String)
solve input = (s1, s2)
  where (state, moves) = parse input
        s1 = part1 state moves -- CNSZFDVLJ
        s2 = part2 state moves -- QNDWLMGNS
