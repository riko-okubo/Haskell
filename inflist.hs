inflist :: Int -> [Int]
inflist n = n : inflist (n + 1)