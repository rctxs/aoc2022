import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  print $ taskOne f1
  print $ taskTwo f1

taskOne :: String -> Int
taskOne = sum . map (prioritize . dubbleItem) . lines

taskTwo :: String -> Int
taskTwo = sum . map (prioritize . commonItems) . chunksOf 3 . lines

items = ['a' .. 'z'] ++ ['A' .. 'Z']

dubbleItem :: String -> Char
dubbleItem line = head [a | a <- take half line, b <- drop half line, a == b]
  where
    half = div (length line) 2

prioritize :: Char -> Int
prioritize = (+ 1) . fromJust . flip elemIndex items

commonItems :: [String] -> Char
commonItems xs = head [a | a <- head xs, b <- xs !! 1, a == b, c <- xs !! 2, b == c]