main = do
  contents <- getContents
  putStr (countZeros contents)

signedNewPosition :: String -> Int -> Int
signedNewPosition s position
  | head s == 'R' = position + read (tail s)
  | head s == 'L' = position - read (tail s)

newPosition :: String -> Int -> Int
newPosition s position = mod (signedNewPosition s position) 100

-- add one to offset the "lost" zero if we rotate left. Since something like -1 `quot` 100 = 0, when we actually passed 0
offset :: String -> Int -> Int
offset s position
  | signedNewPosition s position <= 0 && position /= 0 = 1 -- position /= 0 is for we started at 0, something like L1 would put us at -1, but we didn't cross 0 at that step so offset should be 0
  | otherwise = 0

numZeros :: String -> Int -> Int
numZeros s position = abs (signedNewPosition s position `quot` 100) + offset s position

countZeros' :: [String] -> Int -> Int
countZeros' [] _ = 0
countZeros' (s : ss) position = numZeros s position + countZeros' ss (newPosition s position)

countZeros :: String -> String
countZeros s = show (countZeros' (words s) 50)
