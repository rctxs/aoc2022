import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  -- f2 <- readFile "input02.txt"
  print $ taskOne f1
  print $ taskTwo f1

taskOne :: String -> Integer
taskOne = sum . map (parseBool . completePassport . splitPassportStringToTuples) . groupLinesToPassportStrings . lines

taskTwo :: String -> Integer
taskTwo = sum . map (parseBool . completePassport' . splitPassportStringToTuples) . groupLinesToPassportStrings . lines

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

completePassport :: [(String, String)] -> Bool
completePassport xs = complete xs || oneMissingField xs
  where
    complete = (== 8) . length
    oneMissingField xs = length xs == 7 && containsCid xs
    containsCid [] = True
    containsCid (x : xs)
      | fst x == "cid" = False
      | otherwise = containsCid xs

completePassport' :: [(String, String)] -> Bool
completePassport' xs = completePassport xs && all validField xs

validField :: (String, String) -> Bool
validField ("byr", value) = 1920 <= year && year <= 2002 where year = read value
validField ("iyr", value) = 2010 <= year && year <= 2020 where year = read value
validField ("eyr", value) = 2020 <= year && year <= 2030 where year = read value
validField ("hgt", value)
  | length value == 5 && value !! 3 == 'c' = 150 <= heightCm && heightCm <= 193
  | length value == 4 && value !! 2 == 'i' = 59 <= heightIn && heightIn <= 76
  | otherwise = False
  where
    heightCm = read (take 3 value)
    heightIn = read (take 2 value)
validField ("hcl", value) = head value == '#' && all (`elem` (['a' .. 'f'] ++ ['0' .. '9'])) (tail value)
validField ("ecl", value) = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField ("pid", value) = length value == 9 && all (`elem` ['0' .. '9']) value
validField ("cid", _) = True
validField _ = False
