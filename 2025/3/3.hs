import Data.Char (digitToInt)

main = do
  contents <- getContents
  putStrLn (totalJoltage contents)

totalJoltage :: String -> String
totalJoltage contents = show (sum [bankJoltage (parseBank bank) | bank <- words contents])

parseBank :: String -> [Int]
parseBank = map digitToInt

bankJoltage :: [Int] -> Int
bankJoltage bank = (firstVal * 10) + maximum (drop (firstIdx + 1) bank) where (firstVal, firstIdx) = maxIdx (init bank)

tupleMax :: (Ord a) => (a, Int) -> (a, Int) -> (a, Int)
tupleMax (x1, x2) (y1, y2)
  | x1 >= y1 = (x1, x2)
  | otherwise = (y1, y2)

maxIdx :: (Ord a) => [a] -> (a, Int)
maxIdx xs = foldr tupleMax (head xs, 0) (zip xs [0 ..])
