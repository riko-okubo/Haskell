-- f(x) = a0*x^n + a1*x^(n-1) + ... + a(n-1)*x + an

-- 再帰関数を使って多項式を計算する関数を定義してください。
poly :: (Num a) => [a] -> a -> a
poly [] _ = 0
poly (x:xs) n = x * n ^ length xs + poly xs n
