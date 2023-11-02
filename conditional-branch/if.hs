-- 1からnまでの和を返す関数
intSum n = if n == 0 then 0 else n + intSum (n - 1)

-- リストの長さを返す関数
listLen :: [a] -> Int
listLen xs = if null xs then 0 else 1 + listLen (tail xs)

-- ここで null は，与えられたリストが空リストか否かの真偽値を返す組込関数
-- null :: [a] -> Bool

-- 整数のリストの要素の和を求めて返す関数
listSum :: [Int] -> Int
listSum xs = if null xs then 0 else head xs + listSum (tail xs)