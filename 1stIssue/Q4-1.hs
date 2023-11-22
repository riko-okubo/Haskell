import Data.List (nub, sort)

xs :: Int -> Int -> [Int]
xs x k = [x^i | i <- [0..k]]

ys :: Int -> Int -> [Int]
ys y k = [y^j | j <- [0..k]]

zs :: Int -> Int -> [Int]
zs z k = [z^l | l <- [0..k]]

as :: Int -> Int -> Int -> [Int]
as x y z = nub $ sort [x * y * z | x <- xs x 10, y <- ys y 10, z <- zs z 10]

takeXiyjzk :: Int -> Int -> Int -> Int -> [Int]
takeXiyjzk k x y z = take k $ as x y z

