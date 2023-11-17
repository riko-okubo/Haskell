-- f(x) = a0*x^n + a1*x^(n-1) + ... + a(n-1)*x + an

-- foldlを使って多項式を計算する関数を定義してください。
poly :: (Num a) => [a] -> a -> a
poly xs n = foldl (\acc x -> acc * n + x) 0 xs