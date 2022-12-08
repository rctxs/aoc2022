main :: IO ()
main = do
  f <- readFile "input01.txt"
  print $ taskOne f
  print $ taskTwo f

taskOne :: String -> Int
taskOne = length . allVisibleTrees . parseGrid . lines

taskTwo :: String -> Int
taskTwo = maximum . allScenicScores . parseGrid . lines

allVisibleTrees :: [[Int]] -> [(Int, Int)]
allVisibleTrees grid = [(x, y) | x <- [0 .. (length (head grid) - 1)], y <- [0 .. (length grid - 1)], treeVisible x y grid]

treeVisible :: Int -> Int -> [[Int]] -> Bool
treeVisible x y grid = treeVisibleFromTop || treeVisibleFromBottom || treeVisibleFromLeft || treeVisibleFromRight
  where
    treeSize = (grid !! y) !! x
    treeVisibleFromTop = (all (< treeSize) . take y . map (!! x)) grid
    treeVisibleFromBottom = (all (< treeSize) . drop (y + 1) . map (!! x)) grid
    treeVisibleFromLeft = (all (< treeSize) . take x . (!! y)) grid
    treeVisibleFromRight = (all (< treeSize) . drop (x + 1) . (!! y)) grid

allScenicScores :: [[Int]] -> [Int]
allScenicScores grid = [scenicScore x y grid | x <- [0 .. (length (head grid) - 1)], y <- [0 .. (length grid - 1)]]

scenicScore :: Int -> Int -> [[Int]] -> Int
scenicScore x y grid = top * bottom * left * right
  where
    treeSize = (grid !! y) !! x
    top = (countVisibleTrees . reverse . take y . map (!! x)) grid
    bottom = (countVisibleTrees . drop (y + 1) . map (!! x)) grid
    left = (countVisibleTrees . reverse . take x . (!! y)) grid
    right = (countVisibleTrees . drop (x + 1) . (!! y)) grid
    countVisibleTrees trees = if nrOfSmallerTrees < nrOfTreesToEdge then nrOfSmallerTrees + 1 else nrOfSmallerTrees
      where
        nrOfTreesToEdge = length trees
        nrOfSmallerTrees = (length . takeWhile (< treeSize)) trees

parseGrid :: [String] -> [[Int]]
parseGrid = map (map (\x -> read [x] :: Int))