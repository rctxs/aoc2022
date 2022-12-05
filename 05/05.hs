import Data.List.Split.Internals (splitOn, splitWhen)

main :: IO ()
main = do
  f1 <- readFile "input01.txt"

  print $ taskOne f1
  print $ taskTwo f1

taskOne :: String -> [Char]
taskOne = map head . uncurry (foldl (flip applyProcedureCrateMoover9000)) . parseFile

taskTwo :: String -> [Char]
taskTwo = map head . uncurry (foldl (flip applyProcedureCrateMoover9001)) . parseFile

applyProcedureCrateMoover9000 :: (Int, Int, Int) -> [[Char]] -> [[Char]]
applyProcedureCrateMoover9000 (0, _, _) crateStacks = crateStacks
applyProcedureCrateMoover9000 (n, start, destination) crateStacks = applyProcedureCrateMoover9000 (n - 1, start, destination) crateStacks'
  where
    topElem = (head . (!! (start - 1))) crateStacks
    crateStacks' = (addCrate 1 destination topElem . removeCrate 1 start) crateStacks

    removeCrate i start crateStacks
      | i == start = (tail . head) crateStacks : tail crateStacks
      | otherwise = head crateStacks : removeCrate (i + 1) start (tail crateStacks)

    addCrate i destination newElem crateStacks
      | i == destination = (newElem : head crateStacks) : tail crateStacks
      | otherwise = head crateStacks : addCrate (i + 1) destination newElem (tail crateStacks)

applyProcedureCrateMoover9001 :: (Int, Int, Int) -> [[Char]] -> [[Char]]
applyProcedureCrateMoover9001 (n, start, destination) crateStacks = crateStacks'
  where
    topElems = (take n . (!! (start - 1))) crateStacks
    crateStacks' = (addCrates 1 destination topElems . removeCrates 1 start) crateStacks

    removeCrates i start crateStacks
      | i == start = (drop n . head) crateStacks : tail crateStacks
      | otherwise = head crateStacks : removeCrates (i + 1) start (tail crateStacks)

    addCrates i destination topElems crateStacks
      | i == destination = (topElems ++ head crateStacks) : tail crateStacks
      | otherwise = head crateStacks : addCrates (i + 1) destination topElems (tail crateStacks)

parseFile :: String -> ([[Char]], [(Int, Int, Int)])
parseFile fileString = (crateStacks, procedure)
  where
    cratesWithProcedure = (splitWhen null . lines) fileString
    crateLines = head cratesWithProcedure
    procedureLines = last cratesWithProcedure
    nrOfStacks = (flip div 3 . length . splitOn " " . last) crateLines
    crateStacks = parseCrateStacks ((tail . reverse) crateLines) (replicate nrOfStacks [])
    procedure = map parseProcedure procedureLines

parseCrateStacks :: [[Char]] -> [[Char]] -> [[Char]]
parseCrateStacks [] crateStacks = crateStacks
parseCrateStacks (level : crateLevel) crateStacks = parseCrateStacks crateLevel crateStacks'
  where
    crateStacks' = parseCrateLevel level crateStacks

parseCrateLevel :: [Char] -> [[Char]] -> [[Char]]
parseCrateLevel [] crateStacks = crateStacks
parseCrateLevel crateLevel crateStacks = currentCrateStack' : parseCrateLevel crateLevel' crateStacks'
  where
    currentCrateStack = head crateStacks
    nextCrateElement = (!! 1) crateLevel
    currentCrateStack' = if nextCrateElement /= ' ' then nextCrateElement : currentCrateStack else currentCrateStack
    crateLevel' = drop 4 crateLevel
    crateStacks' = if crateStacks /= [] then tail crateStacks else []

parseProcedure :: [Char] -> (Int, Int, Int)
parseProcedure line = (amount, start, destination)
  where
    lineSplit = splitOn " " line
    parseInt = \x -> read x :: Int
    amount = (parseInt . (!! 1)) lineSplit
    start = (parseInt . (!! 3)) lineSplit
    destination = (parseInt . last) lineSplit
