import Data.List
import Data.Maybe

produtoEscalar xs ys = foldl (\acc (x,y) -> (x*y) + acc) 0 $ zip xs ys



indices x xs = foldl (\ys (a,b) -> if a == x then ys ++ [b] else ys) [] $ zip xs [0..]



concatenaMap x xs = foldr (\ys a -> ys ++ a) [] $ map x xs



ehPrimo x = if primeVal x == 2 then True else False
    where primeVal x = foldl (\acc (a,b) -> if a `mod` b == 0 then acc+1 else acc) 0 $ zip [x,x..] [1..x]




converterInd x
    | x > 25    = x-26
    | otherwise = x

infiniteStg xs = xs ++ infiniteStg xs

equiv = take 26 ['A','B'..]
conversor [] = []
conversor xs = [fromJust $ elemIndex (head xs) equiv] ++ conversor (tail xs)

vigenere' xs ys = zip (conversor xs) (conversor (infiniteStg ys))

newString xs ys = [ a + b | (a,b) <- (vigenere' xs ys)]

builder [] = []
builder xs = [equiv !! converterInd (head xs)] ++ builder (tail xs)

vigenere xs ys = builder $ newString xs ys



text = ['0'..'9'] ++ ['A'..'Z']
base 0 x = []
base x y = base (x `div` y) y ++ [text !! (x `mod` y)]


reduce' x y 0 = 1
reduce' x y z = if x `mod` z == 0 && y `mod` z == 0 then z else reduce' x y (z-1)

reduce (x,y)
    | x > y     = (x `div` (reduce' x y x),y `div` (reduce' x y x)) 
    | otherwise = (x `div` (reduce' x y y),y `div` (reduce' x y y))