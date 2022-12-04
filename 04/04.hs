import Data.List.Split

main = do
  f1 <- readFile "input01.txt"
  print $ taskOne f1
  print $ taskTwo f1

taskOne = length . filter overlap . map (parseLine) . lines
taskTwo = length . filter overlap' . map (parseLine) . lines

overlap ((a, b), (a', b')) = a <= a' && b' <= b || a' <= a && b <= b'
overlap' ((a, b), (a', b')) = 0 /= (length [e | e<-[a..b], e'<-[a'..b'], e==e'])

parseLine line = ((a, b), (a', b'))
    xline = ((map (map (\x -> read x :: Int))) . map (splitOn "-") . splitOn ",") line
    a = (head . head) xline
    b = (last . head) xline
    a' = (head . last) xline
    b' = (last . last) xline