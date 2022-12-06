{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

main :: IO ()
main = do
  file <- readFile "input01.txt"
  print $ startOfPacketMarkerPos file 4
  print $ startOfPacketMarkerPos file 14

startOfPacketMarkerPos :: String -> Int -> Int
startOfPacketMarkerPos = helper 0
  where
    helper charactersBeforeMarker datastream lengthOfMarker
      | distinctCharacters (take lengthOfMarker datastream) = charactersBeforeMarker + lengthOfMarker
      | otherwise = helper (charactersBeforeMarker + 1) (tail datastream) lengthOfMarker
    distinctCharacters xs = length xs == length [a | a <- xs, b <- xs, a == b]
