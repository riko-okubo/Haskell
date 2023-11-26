myTake :: Int -> [a] -> [a]
myTake n xs
  | n <= 0 || null xs = []  
  | n >= length xs = xs    
  | otherwise = take n xs 