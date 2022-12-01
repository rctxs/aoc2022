import Data.List (sort)

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  print $ taskOne f1
  print $ taskTwo f1

taskOne :: String -> Int
taskOne = maximum . map sum . parseElves

taskTwo :: String -> Int
taskTwo = sum . take 3 . reverse . sort . map sum . parseElves

parseElves :: String -> [[Int]]
parseElves = parseLine [] [] . lines
  where
    parseLine :: [Int] -> [[Int]] -> [String] -> [[Int]]
    -- end of file reached
    parseLine elve elves [] = elve : elves
    -- blank line
    parseLine elve elves ("" : xs) = parseLine [] (elve : elves) xs
    -- line with number
    parseLine elve elves (line : xs) = parseLine (read line : elve) elves xs