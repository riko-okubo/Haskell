import Data.Eq

bar :: (Eq a) => a -> a -> b -> b -> b
bar x y p q = if x == y then p else q
-- 「(Eq a) =>」は，「型 a は型クラス (型の集合) Eq に含まれていなければならない」という意味．

