import Data.Char (digitToInt)

main = do
  contents <- getContents
  putStrLn (totalJoltage contents)

totalJoltage :: String -> String
totalJoltage contents = show (sum [bankJoltage (parseBank bank) | bank <- words contents])

parseBank :: String -> [Int]
parseBank = map digitToInt

bankJoltage' :: [Int] -> Int -> Int
bankJoltage' _ 0 = 0
bankJoltage' bank batteryNum = firstVal * 10 ^ (batteryNum - 1) + bankJoltage' (drop (firstIdx + 1) bank) (batteryNum - 1) where (firstVal, firstIdx) = maxIdx (take (length bank - batteryNum + 1) bank)

bankJoltage :: [Int] -> Int
bankJoltage bank = bankJoltage' bank 12

tupleMax :: (Ord a) => (a, Int) -> (a, Int) -> (a, Int)
tupleMax (x1, x2) (y1, y2)
  | x1 >= y1 = (x1, x2)
  | otherwise = (y1, y2)

maxIdx :: (Ord a) => [a] -> (a, Int)
maxIdx xs = foldr tupleMax (head xs, 0) (zip xs [0 ..])
