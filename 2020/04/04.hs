import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  -- f2 <- readFile "input02.txt"
  print $ taskOne f1

--print $ taskTwo f2

--taskOne :: String -> Integer
taskOne = sum . map (parseBool . validatePassport . splitPassportStringToTuples) . groupLinesToPassportStrings . lines

--taskOne = map (validatePassport . splitPassportStringToTuples) . groupLinesToPassportStrings . lines

--taskTwo :: String -> Maybe String
--taskTwo _ = Nothing

parseBool :: Bool -> Integer
parseBool b = if b then 1 else 0

groupLinesToPassportStrings :: [String] -> [String]
groupLinesToPassportStrings = reverse . helper "" []
  where
    helper :: String -> [String] -> [String] -> [String]
    helper current passports [] = current : passports
    helper current passports ("" : lines) = helper "" (current : passports) lines
    helper current passports (l : lines) = helper (trim (l ++ " " ++ current)) passports lines

    trim :: String -> String
    trim xs
      | head xs == ' ' = trim (tail xs)
      | last xs == ' ' = trim (init xs)
      | otherwise = xs

splitPassportStringToTuples :: String -> [(String, String)]
splitPassportStringToTuples = map (makeTuple . splitOn ":") . splitOn " "
  where
    makeTuple xs = (head xs, last xs)

validatePassport :: [(String, String)] -> Bool
validatePassport xs = complete xs || oneMissingField xs
  where
    complete = (== 8) . length
    oneMissingField xs = length xs == 7 && containsCid xs
    containsCid [] = True
    containsCid (x : xs)
      | fst x == "cid" = False
      | otherwise = containsCid xs
