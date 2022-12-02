import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  print $ taskOne f1
  print $ taskTwo f1

taskOne :: String -> Int
taskOne = sum . map (roundScore . parseLine) . lines

taskTwo :: String -> Int
taskTwo = sum . map (roundScore' . parseLine) . lines

parseLine :: String -> (Char, Char)
parseLine line = (opponent, you)
  where
    opponent = (head . head . splitOn " ") line
    you = (head . last . splitOn " ") line

roundScore :: (Char, Char) -> Int
roundScore (opponent, you) = choosingScore you + outcomeScore opponent you
  where
    choosingScore 'X' = 1
    choosingScore 'Y' = 2
    choosingScore 'Z' = 3
    --                          elve vs you
    choosingScore _ = 0
    outcomeScore 'A' 'X' = 3
    outcomeScore 'B' 'Y' = 3
    outcomeScore 'C' 'Z' = 3
    outcomeScore 'A' 'Y' = 6 -- rock vs paper
    outcomeScore 'B' 'X' = 0 -- paper vs rock
    outcomeScore 'B' 'Z' = 6 -- paper vs scissors
    outcomeScore 'C' 'Y' = 0 -- scissors vs paper
    outcomeScore 'C' 'X' = 6 -- scissors vs rock
    outcomeScore 'A' 'Z' = 0 -- rock vs scissors
    outcomeScore _ _ = 0

--        |---- rock <----
--        v              |
--    scissors ------> paper

roundScore' :: (Char, Char) -> Int
roundScore' (opponent, outcome) = choosingScore opponent outcome + outcomeScore outcome
  where
    outcomeScore 'X' = 0
    outcomeScore 'Y' = 3
    outcomeScore 'Z' = 6
    outcomeScore _ = 0

    -- choosing score: rock < paper < scissors
    --                           elve vs you
    choosingScore 'A' 'X' = 3 -- rock vs scissors
    choosingScore 'B' 'X' = 1 -- paper vs rock
    choosingScore 'C' 'X' = 2 -- scissors vs paper
    choosingScore 'A' 'Y' = 1 -- rock vs rock
    choosingScore 'B' 'Y' = 2 -- paper vs paper
    choosingScore 'C' 'Y' = 3 -- scissors vs scissors
    choosingScore 'A' 'Z' = 2 -- rock vs paper
    choosingScore 'B' 'Z' = 3 -- paper vs siossors
    choosingScore 'C' 'Z' = 1 -- scissors vs rock
    choosingScore _ _ = 0