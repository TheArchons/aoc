main = do
  contents <- getContents
  putStr (countZeros contents)

isZero :: Int -> Int
isZero x
  | x == 0 = 1
  | otherwise = 0

newPosition :: String -> Int -> Int
newPosition s position
  | head s == 'R' = mod (position + read (tail s)) 100
  | head s == 'L' = mod (position - read (tail s)) 100

countZeros' :: [String] -> Int -> Int
countZeros' [] _ = 0
countZeros' (s : ss) position = isZero (newPosition s position) + countZeros' ss (newPosition s position)

countZeros :: String -> String
countZeros s = show (countZeros' (words s) 50)
