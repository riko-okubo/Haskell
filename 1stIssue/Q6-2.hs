data BETree a = Empty | BELeaf a |BENode a (BETree a) (BETree a)
    deriving (Show, Eq)
-- 保持する値がa型であるような木をtとする
-- 以下の関数を定義せよ

-- 1. tの最大深さを求める関数depthBETree t
depthBETree :: (Ord a, Num a) => BETree a -> Int
depthBETree Empty = 0
depthBETree (BELeaf _) = 1
depthBETree (BENode _ l r) = 1 + max (depthBETree l) (depthBETree r)

-- 2. tの全ての値の和を求める関数sumBETree t
sumBETree :: (Ord a, Num a) => BETree a -> a
sumBETree Empty = 0
sumBETree (BELeaf x) = x
sumBETree (BENode x l r) = x + sumBETree l + sumBETree r

-- 3. 与えられた木の各ノード以下の部分木の値の和をノードの値として持つような木を作って返す関数upAccBETree t
upAccBETree :: (Ord a, Num a) => BETree a -> BETree a
upAccBETree Empty = Empty
upAccBETree (BELeaf x) = BELeaf x
upAccBETree (BENode x l r) = BENode (x + sumBETree l + sumBETree r) (upAccBETree l) (upAccBETree r)