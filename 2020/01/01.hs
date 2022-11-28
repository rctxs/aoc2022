main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input01.txt"
  print $ taskOne f1
  print $ taskTwo f2

taskOne :: String -> Integer
taskOne = findSum . map read . lines
  where
    findSum xs = head [a * b | a <- xs, b <- xs, a + b == 2020]

taskTwo :: String -> Integer
taskTwo = findSum . map read . lines
  where
    findSum xs =
      head
        [a * b * c | a <- xs, b <- xs, c <- xs, a + b + c == 2020]
