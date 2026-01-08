import Data.Array qualified as Arr
import Data.Char
import Data.Map qualified as Map

main = do
  contents <- getContents
  print (numExitsWithBoth (parseContents contents) (strToInt "svr"))

-- print (parseContents contents)

strToInt :: String -> Int
strToInt = foldl (\l r -> l * 100 + ord r - ord 'a') 0

sumQuadTuple :: [(Int, Int, Int, Int)] -> (Int, Int, Int, Int)
sumQuadTuple = foldr (\(l1, l2, l3, l4) (r1, r2, r3, r4) -> (l1 + r1, l2 + r2, l3 + r3, l4 + r4)) (0, 0, 0, 0)

numExitsWithBoth :: Map.Map Int [Int] -> Int -> Int
numExitsWithBoth map position = total where (_, _, _, total) = numExits map position

-- (numexits, numdacs, numfft, numboth)
numExits :: Map.Map Int [Int] -> Int -> (Int, Int, Int, Int)
numExits map position = memo Arr.! position
  where
    memo = Arr.array (0, strToInt "zzz") [(pos, numExits' pos) | pos <- [0 .. (strToInt "zzz")]]
    numExits' position
      | position == strToInt "out" = (1, 0, 0, 0)
      | position == strToInt "dac" = (t1, t1, t3, min t1 t3)
      | position == strToInt "fft" = (t1, t2, t1, min t1 t2)
      | otherwise = (t1, t2, t3, t4)
      where
        (t1, t2, t3, t4) = sumQuadTuple [memo Arr.! nextPosition | nextPosition <- map Map.! position]

maybeGetFromMap :: Int -> Map.Map Int [Int] -> [Int]
maybeGetFromMap key map
  | Map.member key map = map Map.! key
  | otherwise = []

addServerToMap :: [String] -> Map.Map Int [Int] -> Map.Map Int [Int]
addServerToMap line currMap = foldr (\l r -> Map.insert start (strToInt l : maybeGetFromMap start r) r) currMap (tail line) where start = strToInt (init (head line))

parseContents :: String -> Map.Map Int [Int]
parseContents contents = foldr (addServerToMap . words) Map.empty (lines contents)
