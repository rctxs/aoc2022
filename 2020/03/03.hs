main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  print $ taskOne f1
  print $ taskTwo f1

taskOne :: String -> Int
taskOne = tobogann 3 1 . infiniteArea . lines

taskTwo :: String -> Int
taskTwo file = product [a, b, c, d, e]
  where
    tobogann' x y = (tobogann x y . infiniteArea . lines) file
    a = tobogann' 1 1
    b = tobogann' 3 1
    c = tobogann' 5 1
    d = tobogann' 7 1
    e = tobogann' 1 2

infiniteArea :: [String] -> [String]
infiniteArea = map expandRow

expandRow :: String -> String
expandRow xs = xs ++ expandRow xs

tobogann :: Int -> Int -> [String] -> Int
tobogann slope_x slope_y = helper 0
  where
    helper n [] = n
    helper n xs
      | (head . head) xs == '#' = helper (n + 1) tailRows
      | otherwise = helper n tailRows
      where
        tailLines = drop slope_y xs
        tailRows = map (drop slope_x) tailLines
