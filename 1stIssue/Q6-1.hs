-- 片方の部分木しかないこともあり得る二分木BETree aを定義

data BETree a = Empty | BNode a (BETree a) (BETree a) deriving Show