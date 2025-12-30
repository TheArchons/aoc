main = do
  contents <- getContents
  print (countFresh (parseInput contents))

inRange :: (Int, Int) -> Int -> Bool
inRange (start, end) id = start <= id && id <= end

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh ranges id = any (`inRange` id) ranges

countFresh :: ([(Int, Int)], [Int]) -> Int
countFresh (ranges, ids) = length [() | id <- ids, isFresh ranges id]

parseInput :: String -> ([(Int, Int)], [Int])
parseInput contents = ([(read start, read end) | range <- ranges, let [start, end] = split range '-'], ids)
  where
    lines = split contents '\n'
    ranges = takeWhile (/= "") lines
    ids = map read (tail (dropWhile (/= "") lines))

silentTail :: [a] -> [a]
silentTail xs
  | null xs = []
  | otherwise = tail xs

split :: String -> Char -> [String]
split "" _ = []
split s sep = takeWhile (/= sep) s : split (silentTail (dropWhile (/= sep) s)) sep
