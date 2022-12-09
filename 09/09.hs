{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (nub) -- remove duplicates
import Data.List.Split (splitOn)

main :: IO ()
main = do
  f <- readFile "input01.txt"
  print $ taskOne f
  print $ taskTwo f

taskOne :: String -> Int
taskOne = length . nub . snd . foldl moveRope (replicate 2 (0, 0), [(0, 0)]) . concatMap parseMove . lines

taskTwo :: String -> Int
taskTwo = length . nub . snd . foldl moveRope (replicate 10 (0, 0), [(0, 0)]) . concatMap parseMove . lines

data Move = U | L | D | R deriving (Show, Eq)

moveRope :: ([(Int, Int)], [(Int, Int)]) -> Move -> ([(Int, Int)], [(Int, Int)])
moveRope (rope, tailTrail) move = (rope', tailTrail')
  where
    headPos' = (moveHead move . head) rope
    rope' = (scanl moveTail headPos' . tail) rope
    tailTrail' = last rope' : tailTrail

moveHead :: Move -> (Int, Int) -> (Int, Int)
moveHead U (x, y) = (x, y + 1)
moveHead L (x, y) = (x - 1, y)
moveHead D (x, y) = (x, y - 1)
moveHead R (x, y) = (x + 1, y)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (xHead, yHead) (xTail, yTail)
  | abs (xHead - xTail) <= 1 && abs (yHead - yTail) <= 1 = (xTail, yTail) -- head and tail are adjacent or on the same position -> no tail movement
  | xHead == xTail && yHead > yTail = (xTail, yTail + 1) -- same column, head over tail
  | xHead == xTail && yHead < yTail = (xTail, yTail - 1) -- same column, head under tail
  | yHead == yTail && xHead > xTail = (xTail + 1, yTail) -- same row, head right of tail
  | yHead == yTail && xHead < xTail = (xTail - 1, yTail) -- same row, head left of tail
  | xHead > xTail && yHead > yTail = (xTail + 1, yTail + 1) -- head top right of tail
  | xHead < xTail && yHead > yTail = (xTail - 1, yTail + 1) -- head top left of tail
  | xHead < xTail && yHead < yTail = (xTail - 1, yTail - 1) -- head down left of tail
  | xHead > xTail && yHead < yTail = (xTail + 1, yTail - 1) -- head down right of tail

parseMove :: String -> [Move]
parseMove line
  | direction == 'U' = replicate steps U
  | direction == 'L' = replicate steps L
  | direction == 'D' = replicate steps D
  | direction == 'R' = replicate steps R
  where
    direction = (head . head) split
    steps = ((\x -> read x :: Int) . last) split
    split = splitOn " " line