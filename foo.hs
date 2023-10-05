foo :: Int -> Int -> Int
foo n = \x -> n * x ^ n

-- foo :: Int -> Int -> Int
-- foo n x = n * x ^ n
-- これも同じ意味

foo2 :: Int -> Int
foo2 = foo 2         -- nとして2が与えられた関数

foo3 :: Int -> Int
foo3 = foo 3        -- nとして3が与えられた関数
