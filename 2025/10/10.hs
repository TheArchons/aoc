import Data.Bits (Bits (xor))
import Data.Map qualified as Map

main = do
  contents <- getContents
  print (totalNumClicks (parseContents contents))

parseIndicator :: Char -> Int
parseIndicator indicator
  | indicator == '#' = 1
  | otherwise = 0

parseIndicators :: String -> Int
parseIndicators = foldr (\l r -> parseIndicator l + r * 2) 0

wiresToInt :: [Int] -> Int
wiresToInt wires = sum [2 ^ n | n <- wires]

parseButtons :: [String] -> [Int]
parseButtons = map (wiresToInt . map read . (`split` ',') . tail . init)

parseLine :: [String] -> (Int, [Int])
parseLine (indicators : buttons) = (parseIndicators (tail (init indicators)), parseButtons buttons)

parseContents :: String -> [(Int, [Int])]
parseContents contents = map (parseLine . init . words) (lines contents)

pressButton :: Int -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int
pressButton current minValue button minValues
  | Map.member (current `xor` button) minValues = minValues
  | otherwise = Map.insert (current `xor` button) (minValue + 1) minValues

pressButtonsForValue :: [Int] -> (Int, Int) -> Map.Map Int Int -> Map.Map Int Int
pressButtonsForValue buttons (current, minValue) minValues = foldr (pressButton current minValue) minValues buttons

numClicks :: (Int, [Int]) -> Map.Map Int Int -> Int
numClicks (target, buttons) minValues
  | Map.member target minValues = minValues Map.! target
  | otherwise = numClicks (target, buttons) (foldr (pressButtonsForValue buttons) minValues (Map.assocs minValues))

totalNumClicks :: [(Int, [Int])] -> Int
totalNumClicks machines = sum (map (`numClicks` Map.fromList [(0, 0)]) machines)

silentTail :: [a] -> [a]
silentTail xs
  | null xs = []
  | otherwise = tail xs

split :: String -> Char -> [String]
split "" _ = []
split s sep = takeWhile (/= sep) s : split (silentTail (dropWhile (/= sep) s)) sep
