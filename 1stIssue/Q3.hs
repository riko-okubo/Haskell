import Data.List (nub, sort)

merge3 :: (Ord a) => [a] -> [a] -> [a] -> [a]
merge3 xs ys zs = nub $ sort $ xs ++ ys ++ zs

