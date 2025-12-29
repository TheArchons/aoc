main = do
  contents <- getContents
  print (numRolls (parseContents contents))

between :: (Ord a) => a -> a -> a -> Bool
between a b c = a < b && b < c

isAccessible :: Int -> Int -> [[Bool]] -> Bool
isAccessible row col rolls =
  sum
    [fromEnum (rolls !! (row + r) !! (col + c)) | r <- [-1, 0, 1], between (-1) (row + r) (length rolls), c <- [-1, 0, 1], between (-1) (col + c) (length (head rolls)), r /= 0 || c /= 0]
    < 4

numRolls :: [[Bool]] -> Int
numRolls rolls =
  sum
    [ fromEnum (isAccessible row col rolls)
      | row <- [0 .. (length rolls - 1)],
        col <- [0 .. (length (head rolls) - 1)],
        rolls !! row !! col
    ]

parseContents :: String -> [[Bool]]
parseContents contents = [[c == '@' | c <- row] | row <- words contents]
