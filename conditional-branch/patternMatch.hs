import Distribution.Simple.Program.HcPkg (list)
-- リストの長さを返す関数
listLen [] = 0
listLen (x:xs) = 1 + listLen xs

-- 整数のリストの要素の和を求めて返す関数
listSum [] = 0 
listSum (x:xs) = x + listSum xs