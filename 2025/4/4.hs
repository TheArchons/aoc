main = do
  contents <- getContents
  print (numRemovable (parseContents contents))

between :: (Ord a) => a -> a -> a -> Bool
between a b c = a < b && b < c

isAccessible :: Int -> Int -> [[Bool]] -> Bool
isAccessible row col rolls =
  sum
    [fromEnum (rolls !! (row + r) !! (col + c)) | r <- [-1, 0, 1], between (-1) (row + r) (length rolls), c <- [-1, 0, 1], between (-1) (col + c) (length (head rolls)), r /= 0 || c /= 0]
    < 4

findRemovable :: [[Bool]] -> [(Int, Int)]
findRemovable rolls =
  [ (row, col)
    | row <- [0 .. (length rolls - 1)],
      col <- [0 .. (length (head rolls) - 1)],
      rolls !! row !! col,
      isAccessible row col rolls
  ]

remove :: [[Bool]] -> [(Int, Int)] -> [[Bool]]
remove rolls removable = [[col && (rowIdx, colIdx) `notElem` removable | (col, colIdx) <- zip row [0 ..]] | (row, rowIdx) <- zip rolls [0 ..]]

numRemovable :: [[Bool]] -> Int
numRemovable rolls =
  if not (null removable)
    then length removable + numRemovable (remove rolls removable)
    else 0
  where
    removable = findRemovable rolls

parseContents :: String -> [[Bool]]
parseContents contents = [[c == '@' | c <- row] | row <- words contents]
