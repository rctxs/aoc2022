{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Eq (Eq)
import Data.List.Split.Internals (splitOn)

main :: IO ()
main = do
  f <- readFile "input01.txt"
  print $ taskOne f
  print $ taskTwo f

-- sum the size of all directories with size <= 100000
taskOne :: String -> Int
taskOne = sum . filter (<= 100000) . map fileTreeSize . flatDirList . head . fst . parseFileTrees . parseTerminalLines

-- find the size of the smallest directory to delete to make 30000000 units of disk space available
taskTwo :: String -> Int
taskTwo fileString = minSize
  where
    fileTree = (head . fst . parseFileTrees . parseTerminalLines) fileString
    usedSpace = fileTreeSize fileTree
    freeSpace = 70000000 - usedSpace
    overAllocatedSpace = 30000000 - freeSpace
    deletionCandidateSizes = (filter (>= overAllocatedSpace) . map fileTreeSize . flatDirList) fileTree
    minSize = minimum deletionCandidateSizes

data FileTree = Dir Int String [FileTree] | File Int String
  deriving (Show)

data TerminalLine = CD String | CD_BACK | LS | OUTPUT FileTree
  deriving (Show)

isOutput :: TerminalLine -> Bool
isOutput (OUTPUT _) = True
isOutput _ = False

isFile :: FileTree -> Bool
isFile (File _ _) = True
isFile _ = False

fileTreeSize :: FileTree -> Int
fileTreeSize (Dir size _ _) = size
fileTreeSize (File size _) = size

toFileTree :: TerminalLine -> FileTree
toFileTree (OUTPUT a) = a
toFileTree _ = error "No FileTree"

flatDirList :: FileTree -> [FileTree]
flatDirList (Dir size name dirList) = Dir size name [] : concatMap flatDirList dirList
flatDirList (File _ _) = []

-- assumptions:
-- - each "$ cd dir" is direclty followed by "$ ls"
-- - no directory is visited more then once
parseFileTrees :: [TerminalLine] -> ([FileTree], [TerminalLine])
parseFileTrees [] = ([], [])
parseFileTrees (CD_BACK : terminalLog) = ([], terminalLog)
parseFileTrees (CD dirName : LS : terminalLog) = (fileTreesOfParentDir, followingTerminalLogOfParentDir)
  where
    -- parse files and their sizes
    dirContent = (map toFileTree . takeWhile isOutput) terminalLog
    fileList = filter isFile dirContent
    fileSize = (sum . map fileTreeSize) fileList

    -- recursively parse subdirectories
    followingTerminalLogWithSubDirs = dropWhile isOutput terminalLog
    (subDirList, followingTerminalLogAfterSubDirs) = parseFileTrees followingTerminalLogWithSubDirs

    -- assamble this directory with all files and subdirectories and their sizes
    subDirSize = (sum . map fileTreeSize) subDirList
    dirList = fileList ++ subDirList
    dirSize = fileSize + subDirSize
    thisDir = Dir dirSize dirName dirList

    -- return the list of filetrees of the parent directory AND the unused terminal log
    parseResultOfDirsAfterThisDir = parseFileTrees followingTerminalLogAfterSubDirs
    fileTreesOfParentDir = thisDir : fst parseResultOfDirsAfterThisDir
    followingTerminalLogOfParentDir = snd parseResultOfDirsAfterThisDir

parseTerminalLine :: [Char] -> TerminalLine
parseTerminalLine terminalLine
  -- parse terminal commands
  | head terminalLine == '$' = if splitTerminalLine !! 1 == "cd" then currentCD else LS
  | take 3 terminalLine == "dir" = OUTPUT (Dir 0 (last splitTerminalLine) [])
  -- parse terminal output
  | otherwise = OUTPUT (File ((\x -> read x :: Int) (head splitTerminalLine)) (last splitTerminalLine))
  where
    splitTerminalLine = splitOn " " terminalLine
    currentCD = if last splitTerminalLine == ".." then CD_BACK else CD (last splitTerminalLine)

parseTerminalLines :: String -> [TerminalLine]
parseTerminalLines = map parseTerminalLine . lines