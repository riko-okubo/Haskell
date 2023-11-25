msp :: (Ord a, Num a) => [a] -> a
msp = msp2 1 1
  where
    msp2 s msp [] = msp
    msp2 s msp (x:xs) = msp2 (max x (s * x)) (max msp (s * x)) xs
