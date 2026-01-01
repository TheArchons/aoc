import Data.List
import Data.Maybe
import Data.Set

main = do
  contents <- getContents
  print (numBeams (words contents))

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
