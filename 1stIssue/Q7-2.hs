-- 自然な再帰によりリストを生成する汎用的な高階関数andListを定義する
-- 関数3つ（p,f,g）とリストを作成する元となるデータxを引数にとる
-- p x が真ならば、空リスト[]を返す。
-- 偽であれば佐伯呼び出しによりconsセルを生成する。
-- 生成されたconsセルのhead部はf xにより作り、
-- 再帰呼び出しで渡すデータ（第4引数）はg xにより作る。

andList :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
andList p f g x
  | p x = []
  | otherwise = f x : andList p f g (g x)

-- andListを使って、0以上の整数nを受け取り、リスト[n, n-1, ... ,0]を返す関数を定義する。
downto0 :: Int -> [Int]
downto0 = andList p f g
  where
    p x = x < 0
    f x = x
    g x = x - 1

-- andListを使って、二つの要素がa型のリストのペアを引数にとり、対応する同士の和からなるリストを返す関数を定義する。
-- ただし、二つのリストの長さが異なる場合は、短い方のリストの長さに合わせる。
zipPlus :: (Num a) => ([a],[a]) -> [a]
zipPlus = andList p f g
  where
    p (x, y) = null x || null y
    f (x, y) = head x + head y
    g (x, y) = (tail x, tail y)