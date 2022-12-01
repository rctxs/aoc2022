import Data.List (sort)

main = do
  f <- readFile "input01.txt"
  print $ t1 f
  print $ t2 f
  where
    t1 = maximum . map sum . p
    t2 = sum . take 3 . reverse . sort . map sum . p
    p = h [] [] . lines
    h e es [] = e : es
    h e es ("" : xs) = h [] (e : es) xs
    h e es (l : xs) = h (read l : e) es xs
