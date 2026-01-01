import Data.List
import Data.Maybe
import Data.Set

main = do
  contents <- getContents
  print (numTimelines (parseContents contents) (fromJust (elemIndex 'S' contents)) 0)

addToSet :: (Set Int, Int) -> Char -> (Set Int, Int)
addToSet (currSet, idx) char
  | char == '^' = (Data.Set.insert idx currSet, idx + 1)
  | otherwise = (currSet, idx + 1)

calculateNewSet :: String -> Set Int
calculateNewSet line = fst (Data.List.foldl addToSet (Data.Set.empty, 0) line)

findSplitterSet :: [String] -> [Set Int]
findSplitterSet lines = [calculateNewSet line | line <- lines]

parseContents :: String -> [Set Int]
parseContents contents = findSplitterSet (tail (words contents))

numTimelines :: [Set Int] -> Int -> Int -> Int
numTimelines lines p' l' = memo !! p' !! l'
  where
    memo = [[self p l | l <- [0 ..]] | p <- [0 ..]]

    self position lineNum
      | length lines == lineNum = 1
      | member position (lines !! lineNum) = memo !! (position - 1) !! (lineNum + 1) + memo !! (position + 1) !! (lineNum + 1)
      | otherwise = memo !! position !! (lineNum + 1)

initialSet :: [String] -> Set Int
initialSet lines = fromList [fromJust (elemIndex 'S' (head lines))]

calculateSplit :: (Set Int, Int, Int) -> Char -> (Set Int, Int, Int)
calculateSplit (set, idx, numSplit) c
  | c == '^' && member idx set = (Data.Set.delete idx (Data.Set.insert (idx + 1) (Data.Set.insert (idx - 1) set)), idx + 1, numSplit + 1)
  | otherwise = (set, idx + 1, numSplit)

calculateNextSet :: (Set Int, Int) -> String -> (Set Int, Int)
calculateNextSet (currentSet, numSplit) line = (nextSet, newNumSplit) where (nextSet, _, newNumSplit) = Data.List.foldl calculateSplit (currentSet, 0, numSplit) line

numBeams :: [String] -> Int
numBeams lines = snd (Data.List.foldl calculateNextSet (initialSet lines, 0) (tail lines))
