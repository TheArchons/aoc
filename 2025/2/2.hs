main = do
  contents <- getContents
  putStrLn (sumInvalids contents)

getRange :: [String] -> [(Int, Int)]
getRange ranges = [(read (head rangeList), read (last rangeList)) | range <- ranges, let rangeList = split range '-']

repeatsWith :: String -> String -> Bool
repeatsWith "" _ = True
repeatsWith s rep = take (length rep) s == rep && repeatsWith (drop (length rep) s) rep

isInvalid :: Int -> Bool
isInvalid x = any (\n -> repeatsWith strX (take n strX)) [1 .. (length strX `div` 2)] where strX = show x

numInvalids :: (Int, Int) -> Int
numInvalids (start, end)
  | start > end = 0
  | isInvalid start = start + numInvalids (start + 1, end)
  | otherwise = numInvalids (start + 1, end)

sumInvalids :: String -> String
sumInvalids contents = show (sum [numInvalids range | range <- getRange (split contents ',')])

silentTail :: [a] -> [a]
silentTail xs
  | null xs = []
  | otherwise = tail xs

split :: String -> Char -> [String]
split "" _ = []
split s sep = takeWhile (/= sep) s : split (silentTail (dropWhile (/= sep) s)) sep
