data BETree a = Empty | BELeaf a | BENode a (BETree a) (BETree a) deriving Show
cataBETree :: (Ord a, Num a) => (BETree a -> b) -> (a -> b -> b -> b) -> BETree a -> b
cataBETree f g Empty = f Empty
cataBETree f g (BELeaf x) = f (BELeaf x)
cataBETree f g (BENode x l r) = g x (cataBETree f g l) (cataBETree f g r)

-- 1. tの最大深さを求める関数depthBETree t
depthBETree :: (Ord a, Num a) => BETree a -> Int
depthBETree = cataBETree f g
  where
    f Empty = 0
    f (BELeaf _) = 1
    f (BENode _ _ _) = 1
    g x l r = 1 + max l r

-- 2. tの全ての値の和を求める関数sumBETree t
sumBETree :: (Ord a, Num a) => BETree a -> a
sumBETree = cataBETree f g
  where
    f Empty = 0
    f (BELeaf x) = x
    f (BENode x _ _) = x
    g x l r = x + l + r

-- 3. 与えられた木の各ノード以下の部分木の値の和をノードの値として持つような木を作って返す関数upAccBETree t
upAccBETree :: (Ord a, Num a) => BETree a -> BETree a
upAccBETree = cataBETree f g
  where
    f Empty = Empty
    f (BELeaf x) = BELeaf x
    f (BENode x _ _) = BENode x Empty Empty
    g x l r = BENode (x + sumBETree l + sumBETree r) l r