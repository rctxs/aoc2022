main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1
  print $ taskTwo f2

taskOne :: String -> Maybe String
taskOne _ = Nothing

taskTwo :: String -> Maybe String
taskTwo _ = Nothing