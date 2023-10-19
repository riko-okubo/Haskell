retfst :: Int -> Int -> Int
retfst x y = x

-- 多相関数 (polymorphic function)：種々の型のデータに利用できるような関数

-- EX）与えられた2つの引数のうち，第1引数を返す．ただし，2つの引数の型は何でもよい場合．
polymorphicRetfst :: a -> b -> a
polymorphicRetfst x y = x
