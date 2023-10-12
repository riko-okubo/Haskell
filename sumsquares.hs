bar :: Int
bar = y * z             -- このbarは大域的
    where y = z * 2     -- whereにより局所的な定義を与える
          z = 8         -- 局所的な環境にはy,zがある

zoo :: Int
zoo = let bar = 10 in bar * 5 -- letにより局所的な定義を与える

sumsquares :: Int -> Int -> Int
sumsquares x y = sq x + sq y
    where sq n = n * n  --sq : 局所的に定義された関数

-- sumsquares x y =  let sq n = n * n
--    in sq x + sq y
-- という書き方でも良い（letを用いた書き方）


-- 部分適用（多引数関数のはじめの一部に引数だけを与えたような関数適用）を用いる書き方
-- sumsquares :: Int -> Int -> Int
-- sumsquares = sumfs square 