foo :: Int
foo = 100 

f :: Int -> Int
f x = x + foo 
-- ここでのfooは、大域的なfoo

r :: Int
r = let foo = 10000 in f 1
-- ここでのfooは、大域的なfooではなく、letで定義されたfoo