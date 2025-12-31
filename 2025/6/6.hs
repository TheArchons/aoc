import Data.List (transpose)

main = do
  contents <- getContents
  print (getSum (parseContents contents))

getSum :: [[String]] -> Int
getSum questions = sum [processLine question | question <- questions]

processLine :: [String] -> Int
processLine question =
  if last question == "+"
    then sum nums
    else product nums
  where
    nums = map read (init question)

parseContents :: String -> [[String]]
parseContents contents = transpose (map words (split contents '\n'))

silentTail :: [a] -> [a]
silentTail xs
  | null xs = []
  | otherwise = tail xs

split :: String -> Char -> [String]
split "" _ = []
split s sep = takeWhile (/= sep) s : split (silentTail (dropWhile (/= sep) s)) sep
