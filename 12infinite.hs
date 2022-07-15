import Data.List
import Data.Maybe

myrepeat x = [x] ++ myrepeat x

myreplicate x y = if x > 0 then [y] ++ myreplicate (x-1) y else []



gerador1a = gerador1a' 0
    where gerador1a' x = if x /= 0 then [x] ++ [(-x)] ++ gerador1a' (x+1) else [x] ++ gerador1a' (x+1)

gerador1b = drop 1 gerador1b'
    where gerador1b' = concat[[x,(-x)] | x <- [0..]]

gerador1c = [0] ++ unfoldr (\x -> if x < 0 then Just(x,(-x+1)) else Just(x,(-x))) 1

gerador1d =concat $ map (\(x,y) -> [x,y]) $ zip (iterate (\x -> x-1) 0)(iterate(+1) 1)


gerador2a = gerador2a' 1
    where gerador2a' x = if x > 0 then [x] ++ gerador2a' (-x-1) else [x] ++ gerador2a' (-x+1)

gerador2b = concat[[x, -(x+1)] | x <- [1,3..]]

gerador2c = unfoldr (\x -> if x < 0 then Just(x,(-x+1)) else Just(x,(-x-1))) 1

gerador2d = iterate (\x -> if x < 0 then -x+1 else -x-1) 1


gerador3a = gerador3a' 1
    where gerador3a' x = [x] ++ gerador3a' (x*2)

gerador3b = [ 2^x | x <- [0..]]

gerador3c = unfoldr (\x -> Just(x,(x*2)) ) 1

gerador3d = iterate (*2) 1


gerador4 x = unfoldr (\x -> if x `div` 2 /= 0 || x `mod` 2 /= 0 then Just(x,(x `div` 2)) else Nothing) x



numb x y = unfoldr (\x -> if x `mod` y == 0 then Just((x, x `mod` y),(x `div` y)) else Nothing) x

expoentes x y = length $ numb x y



ehPrimo x = if primeVal x == 2 then True else False
    where primeVal x = foldl (\acc (a,b) -> if a `mod` b == 0 then acc+1 else acc) 0 $ zip [x,x..] [1..x]

primeGen = filter (>0) $ unfoldr (\x -> if ehPrimo x == True then Just(x,(x+1)) else Just(-1,(x+1))) 2

finalDiv x y = if x `mod` y == 0 then finalDiv (x `div` y) y else x

factors' 1 y = []
factors' x y = if (expoentes x (yPrime y)) > 0 then [(yPrime y, expoentes x (yPrime y))] ++ factors' (finalDiv x (yPrime y)) (y+1) else factors' (finalDiv x (yPrime y)) (y+1)
    where yPrime y = primeGen !! y

factors x = factors' x 0


separa 0 = [0]
separa x = reverse $ unfoldr(\a -> if a `div` 10 /= 0 || a `mod` 10 /= 0 then Just(a `mod` 10, a `div` 10) else Nothing) x