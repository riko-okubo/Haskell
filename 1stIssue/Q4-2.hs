import Data.List (nub, sort)

xs :: Int -> Int -> [Int]
xs x k = [x^i | i <- [0..k]]

ys :: Int -> Int -> [Int]
ys y k = [y^j | j <- [0..k]]

zs :: Int -> Int -> [Int]
zs z k = [z^l | l <- [0..k]]

as :: Int -> Int -> Int -> Int -> [Int] 
as k x y z = nub $ sort [x * y * z | x <- xs x k, y <- ys y k, z <- zs z k]

-- 正の整数k1, k2を引数として、k1以上k2以下のasのリストを返す関数を定義してください。
betweenXiYjZk :: Int -> Int -> Int -> Int -> Int -> [Int]
betweenXiYjZk k1 k2 x y z = takeWhile (<= k2) $ dropWhile (< k1) $ as k2 x y z