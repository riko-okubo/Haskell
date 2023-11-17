myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile p (x:xs)
  | p x = myDropWhile p xs
  | otherwise = x:xs