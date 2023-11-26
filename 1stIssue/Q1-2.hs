myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
  | p x = x : myTakeWhile p xs
  | otherwise = []