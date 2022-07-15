import Data.List
import Data.Maybe

sdig 0 = 0
sdig x = x `mod` 10 + sdig (x `div` 10)



rev x = acc x 0
    where acc x y = if x /= 0 then acc (x `div` 10) (y*10 + x `mod` 10) else y

rev2 0 = 0
rev2 x =  x `div` (10^(size x 0)) + (10 * rev2 (x `mod` (10^(size x 0))) )
    where size x y = if x `div` 10 /= 0 then size (x `div` 10) (y+1) else y



--rev3 x = unfoldr(\a -> if a `div` 10 /= 0 || a `mod` 10 /= 0 then Just(a `mod` 10, a `div` 10) else Nothing) x