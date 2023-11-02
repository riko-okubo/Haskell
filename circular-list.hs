import Prelude hiding (take)
take :: (Eq t, Num t) => t -> [a] -> [a]
take n xs =
 if n == 0 then [] else head xs : take (n-1) (tail xs)