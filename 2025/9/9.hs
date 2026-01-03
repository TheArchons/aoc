type Point = (Int, Int)

main = do
  contents <- getContents
  print (largestRectangle (parseContents contents))

area p1 p2 = abs ((fst p1 - fst p2 + 1) * (snd p1 - snd p2 + 1))

largestRectangle points = maximum [area p1 p2 | p1 <- points, p2 <- points, p1 /= p2]

parseLine line = (read (head splitLine), read (last splitLine)) where splitLine = split line ','

parseContents :: String -> [Point]
parseContents contents = [parseLine line | line <- words contents]

silentTail :: [a] -> [a]
silentTail xs
  | null xs = []
  | otherwise = tail xs

split :: String -> Char -> [String]
split "" _ = []
split s sep = takeWhile (/= sep) s : split (silentTail (dropWhile (/= sep) s)) sep
