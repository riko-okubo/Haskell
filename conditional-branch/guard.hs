-- 1からnまでの和を返す関数
intSum n | n == 0 = 0
         | otherwise = n + intSum (n - 1)

-- リストの長さを返す関数
listLen xs | null xs = 0
           | otherwise = 1 + listLen (tail xs)

-- 整数のリストの要素の和を求めて返す関数
listSum xs | null xs = 0
           | otherwise = head xs + listSum (tail xs)
