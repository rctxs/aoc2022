{-# LANGUAGE NamedFieldPuns #-}

import Data.List.Split (splitOn, splitWhen)

main :: IO ()
main = do
  f <- readFile "input01.txt"
  print $ taskOne f
  print $ taskTwo f

taskOne :: String -> Maybe String
taskOne _ = Nothing

taskTwo :: String -> Maybe String
taskTwo _ = Nothing

data Monkey = Monkey
  { number :: Int,
    items :: [Int],
    operation :: Int -> Int,
    divTest :: Int -> Bool,
    targetMonkeyTrue :: Int,
    targetMonkeyFalse :: Int,
    numberOfInspections :: Int
  }

itemsLeft :: Monkey -> Bool
itemsLeft Monkey {items = []} = True
itemsLeft _ = False

addItem :: Monkey -> Int -> Monkey
addItem Monkey {number, items, operation, divTest, targetMonkeyTrue, targetMonkeyFalse, numberOfInspections} newItem =
  Monkey
    { number,
      items = items ++ [newItem],
      operation,
      divTest,
      targetMonkeyTrue,
      targetMonkeyFalse,
      numberOfInspections
    }

inspectItem :: Monkey -> (Monkey, (Int, Int))
inspectItem Monkey {number, items, operation, divTest, targetMonkeyTrue, targetMonkeyFalse, numberOfInspections} = (monkey', (newItem, targetMonkey))
  where
    itemToInspect = head items
    items' = tail items
    newItem = operation itemToInspect
    targetMonkey = if divTest newItem then targetMonkeyTrue else targetMonkeyFalse
    numberOfInspections' = numberOfInspections + 1
    monkey' = Monkey {number, items = items', operation, divTest, targetMonkeyTrue, targetMonkeyFalse, numberOfInspections = numberOfInspections'}

parseMonkeys :: String -> [Monkey]
parseMonkeys = map parseMonkey . splitWhen null . lines

parseMonkey :: [String] -> Monkey
parseMonkey xs =
  Monkey
    { number = (\x -> read x :: Int) . take 1 . last . splitOn " " . (!! 0) $ xs,
      items = map (\x -> read x :: Int) . splitOn "," . last . splitOn ":" . (!! 1) $ xs,
      operation = parseOperation operationStr,
      divTest = (\x -> (== 0) . flip div 3) . (\x -> read x :: Int) . last . splitOn " " . (!! 3) $ xs,
      targetMonkeyTrue = (\x -> read x :: Int) . last . splitOn " " . (!! 4) $ xs,
      targetMonkeyFalse = (\x -> read x :: Int) . last . splitOn " " . (!! 5) $ xs,
      numberOfInspections = 0
    }
  where
    operationStr = last . splitOn "= " . last . splitOn ":" . (!! 2) $ xs
    parseOperation "old * old" = \x -> x * x
    parseOperation xs = if '*' `elem` operationStr then (* operationFaktor) else (+ operationFaktor)
    operationFaktor = (\x -> read x :: Int) . last . splitOn " " $ operationStr
    operationAddend = (\x -> read x :: Int) . last . splitOn " " $ operationStr
