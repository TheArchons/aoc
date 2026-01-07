import Data.Map qualified as Map

main = do
  contents <- getContents
  print (numExits (parseContents contents) "you")

numExits map position
  | position == "out" = 1
  | otherwise = sum [numExits map nextPosition | nextPosition <- map Map.! position]

maybeGetFromMap key map
  | Map.member key map = map Map.! key
  | otherwise = []

addServerToMap line currMap = foldr (\l r -> Map.insert start (l : maybeGetFromMap start r) r) currMap (tail line) where start = init (head line)

parseContents contents = foldr (addServerToMap . words) Map.empty (lines contents)
