-- 高階関数

-- sumfsは関数と2つの整数値を引数に取り、
-- 第1引数の関数を第２、第3引数の整数値に
-- それぞれ適用した結果の和を返す高階関数

sumfs :: (Int -> Int) -> Int -> Int -> Int
sumfs f x y = f x + f y

square :: Int -> Int
square = \n -> n * n        --ここがラムダ式だから、sumfsで関数として使える