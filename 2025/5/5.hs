import Data.List

main = do
  contents <- getContents
  print (numFreshIds (parseInput contents))

numInRange :: Int -> Int -> Int
numInRange start end
  | end - start < 0 = 0
  | otherwise = end - start + 1

calculateStart :: Int -> [(Int, Int)] -> Int -> Int
calculateStart start ranges idx
  | idx == 0 = start
  | otherwise = max start (snd (ranges !! (idx - 1)) + 1)

splitRanges :: [(Int, Int)] -> Int -> [(Int, Int)]
splitRanges [] _ = []
splitRanges ((start, end) : ranges) lastEnd = (max start (lastEnd + 1), end) : splitRanges ranges (max lastEnd end)

numFreshIds :: ([(Int, Int)], a) -> Int
numFreshIds (ranges, _) = sum [numInRange start end | (start, end) <- splitRanges (sortOn fst ranges) (-1)]

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
