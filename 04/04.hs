import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  print $ taskOne f1
  print $ taskTwo f1

taskOne :: String -> Int
taskOne = length . filter overlap . map parseLine . lines

taskTwo :: String -> Int
taskTwo = length . filter overlap' . map parseLine . lines

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a, b), (a', b')) = a <= a' && b' <= b || a' <= a && b <= b'

overlap' :: ((Int, Int), (Int, Int)) -> Bool
overlap' ((a, b), (a', b')) = not (null ([e | e <- [a .. b], e' <- [a' .. b'], e == e']))

parseLine :: [Char] -> ((Int, Int), (Int, Int))
parseLine line = ((a, b), (a', b'))
  where
    xline = (map (map (\x -> read x :: Int) . splitOn "-") . splitOn ",") line
    a = (head . head) xline
    b = (last . head) xline
    a' = (head . last) xline
    b' = (last . last) xline