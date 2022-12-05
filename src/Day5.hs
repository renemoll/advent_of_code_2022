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
                where ys = take 4 xs
                      remainder = drop 4 xs
                    --   line = reverse (ys !! 1)
                      line = (ys !! 1)
          keyValue :: [String] -> [(Int,String)]
          keyValue = map go
            where go xs = (number, filter (not . isSpace) containers)
                     where number = (digitToInt . last) xs
                           containers = init xs
                    --  where number = (digitToInt . head) xs
                    --       containers = tail xs


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

part1 :: State -> [Move] -> String
part1 state moves = top
    where f state m = applyMove state m
          newState = foldl f state moves
          g int v = int ++ [head v]
          top = Map.foldl g [] newState

applyMove2 :: State -> Move -> State
applyMove2 s (n,src,dst) = Map.insert dst new (Map.insert src (drop n l) s)
    where l = s Map.! src
          new = (take n l) ++ (s Map.! dst)

part2 :: State -> [Move] -> String
part2 state moves = top
    where f state m = applyMove2 state m
          newState = foldl f state moves
          g int v = int ++ [head v]
          top = Map.foldl g [] newState

solve :: String -> (String, String)
solve input = (s1, s2)
  where (state, moves) = parse input
        s1 = part1 state moves -- CNSZFDVLJ
        s2 = part2 state moves -- QNDWLMGNS
