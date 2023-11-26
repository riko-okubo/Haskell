-- 中間ノードを保持しない二分木
-- data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)
-- cataTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
-- cataTree f g (Leaf x) = f x
-- cataTree f g (Node l r) = g (cataTree f g l) (cataTree f g r)

-- 中間ノードを保持する二分木
-- data BTree a = BLeaf a | BNode a (BTree a) (BTree a) deriving (Show, Eq)
-- cataBTree :: (a -> b) -> (b -> b -> b -> b) -> BTree a -> b
-- cataBTree f g (BLeaf x) = f x
-- cataBTree f g (BNode x l r) = g x (cataBTree f g l) (cataBTree f g r)

-- BETree a型の木を受け取り、それに対する「自然な再帰」を行う汎用的な高階関数cataBETreeを定義せよ
data BETree a = Empty | BELeaf a | BENode a (BETree a) (BETree a) deriving Show
cataBETree :: (Ord a, Num a) => (BETree a -> b) -> (a -> b -> b -> b) -> BETree a -> b
cataBETree f g Empty = f Empty
cataBETree f g (BELeaf x) = f (BELeaf x)
cataBETree f g (BENode x l r) = g x (cataBETree f g l) (cataBETree f g r)