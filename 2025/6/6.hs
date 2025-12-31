import Data.Char (digitToInt)

main = do
  contents <- getContents
  print (getSum (parseContents contents))

getSum :: [String] -> Int
getSum lines
  | null (head lines) = 0
  | otherwise = solveQuestion lines (head (last lines)) + getSum (tailQuestion lines)

processLine :: [String] -> Int
processLine question =
  if last question == "+"
    then sum nums
    else product nums
  where
    nums = map read (init question)

getFirstColNum :: [[Char]] -> Int
getFirstColNum lines = foldl (\l r -> l * 10 + r) 0 [digitToInt currVal | idx <- [0 .. (length lines - 2)], let currVal = head (lines !! idx), currVal /= ' ']

isEmptyCol :: [String] -> Bool
isEmptyCol = all (\line -> head line == ' ')

solveQuestion :: [String] -> Char -> Int
solveQuestion lines operator
  | null (head lines) || isEmptyCol lines = if operator == '+' then 0 else 1
  | operator == '+' = getFirstColNum lines + solveQuestion (map tail lines) '+'
  | operator == '*' = getFirstColNum lines * solveQuestion (map tail lines) '*'

tailQuestion :: [String] -> [String]
tailQuestion lines
  | null (head lines) = lines
  | isEmptyCol lines = map tail lines
  | otherwise = tailQuestion (map tail lines)

parseContents :: String -> [String]
parseContents contents = split contents '\n'

silentTail :: [a] -> [a]
silentTail xs
  | null xs = []
  | otherwise = tail xs

split :: String -> Char -> [String]
split "" _ = []
split s sep = takeWhile (/= sep) s : split (silentTail (dropWhile (/= sep) s)) sep
