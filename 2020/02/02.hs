import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input01.txt"
  print $ taskOne f1
  print $ taskTwo f2

taskOne :: String -> Integer
taskOne = sum . map ((parseBool . passwordValid) . parseLine) . lines

taskTwo :: String -> Integer
taskTwo = sum . map ((parseBool . passwordValid') . parseLine) . lines

parseBool :: Bool -> Integer
parseBool b = if b then 1 else 0

parseLine :: String -> (Integer, Integer, Char, String)
parseLine xs = (min, max, c, pwd)
  where
    cut01 = splitOn "-" xs
    min = (read . head) cut01
    cut02 = (splitOn " " . head . tail) cut01
    max = (read . head) cut02
    c = head (head (splitOn ":" ((!!) cut02 1)))
    pwd = (!!) cut02 2

passwordValid :: (Integer, Integer, Char, String) -> Bool
passwordValid = helper 0
  where
    helper count (min, max, c, []) = min <= count && count <= max
    helper count (min, max, c, x : xs)
      | x == c = helper (count + 1) (min, max, c, xs)
      | otherwise = helper count (min, max, c, xs)

passwordValid' :: (Integer, Integer, Char, String) -> Bool
passwordValid' = helper False False 1
  where
    helper eq1 eq2 _ (_, _, _, []) = eq1 /= eq2
    helper eq1 eq2 count (pos1, pos2, c, x : xs)
      | count == pos1 = helper (x == c) eq2 (count + 1) (pos1, pos2, c, xs)
      | count == pos2 = helper eq1 (x == c) (count + 1) (pos1, pos2, c, xs)
      | otherwise = helper eq1 eq2 (count + 1) (pos1, pos2, c, xs)
