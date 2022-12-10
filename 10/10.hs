main :: IO ()
main = do
  f <- readFile "input01.txt"
  print $ taskOne f
  putStr $ taskTwo f

taskOne :: String -> Int
taskOne = signalStrengthSum . scanl runInstruction (X 1) . concatMap parseInstruction . lines

taskTwo :: String -> String
taskTwo = cathodeRayTubeStr . flip cathodeRayTube 1 . scanl runInstruction (X 1) . concatMap parseInstruction . lines

data Instruction = Noop | Addx1 | Addx2 Int deriving (Show, Eq)

newtype Register = X Int deriving (Show, Eq)

cathodeRayTubeStr :: String -> String
cathodeRayTubeStr [] = []
cathodeRayTubeStr xs = take 40 xs ++ '\n' : cathodeRayTubeStr (drop 40 xs)

cathodeRayTube :: [Register] -> Int -> String
cathodeRayTube [] _ = []
cathodeRayTube ((X a) : xs) cycle =
  if horizontalSpritePosition `elem` map tunePosition [cycle -1, cycle, cycle + 1]
    then '#' : cathodeRayTube xs (cycle + 1)
    else '.' : cathodeRayTube xs (cycle + 1)
  where
    horizontalSpritePosition = a + 1
    tunePosition pos = pos `mod` 40

signalStrengthSum :: [Register] -> Int
signalStrengthSum xs = (sum . map (\k -> signalStrength (xs !! (k -1)) k)) [20, 60, 100, 140, 180, 220]

signalStrength :: Register -> Int -> Int
signalStrength (X a) cycle = a * cycle

runInstruction :: Register -> Instruction -> Register
runInstruction x Noop = x
runInstruction x Addx1 = x
runInstruction (X a) (Addx2 k) = X (a + k)

parseInstruction :: String -> [Instruction]
parseInstruction "noop" = [Noop]
parseInstruction ('a' : 'd' : 'd' : 'x' : value) = [Addx1, Addx2 (read value :: Int)]
parseInstruction _ = error "parse error, no instruction"