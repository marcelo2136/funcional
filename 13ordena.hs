import Data.List
import Data.Maybe

sublist [x] = [x]
sublist xs = [head xs] ++ [length xs]

compac' [] y = []
compac' xs y
    | head xs /= xs !! y        = [sublist (take y xs)] ++ compac' (drop y xs) 0 
    | null (tail xs) == True    = [sublist xs] ++ []
    | otherwise                 = compac' xs (y+1)

compac xs = compac' xs 0


fib 0 = []
fib 1 = [0]
fib 2 = fib 1 ++ [1]
fib x = fib (x-1) ++ [(fib (x-1) !! (x-2)) + (fib (x-1) !! (x-3))]



ordenada [x] = True
ordenada xs = if head (tail xs) > head xs then ordenada (tail xs) else False



inserir y x xs
    | y <= xs !! x                  = take x xs ++ [y] ++ drop x xs
    | y >= xs !! ((length xs) - 1)  = xs ++ [y]
    | otherwise                     = inserir y (x+1) xs



menores p [] = []
menores p xs = if p > head xs then [head xs] ++ menores p (tail xs) else menores p (tail xs)

maiores p [] = []
maiores p xs = if p <= head xs then [head xs] ++ maiores p (tail xs) else maiores p (tail xs)

separa [x] = [[x]]
separa (x:xs) = [menores x xs] ++ [[x]] ++ [maiores x xs]

qsort [] = []
qsort [x] = [x]
qsort xs = (qsort (separa xs !! 0)) ++ (separa xs !! 1) ++ (qsort (separa xs !! 2))



merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge xs ys = if head xs <= head ys then [head xs] ++ merge (tail xs) ys else [head ys] ++ merge xs (tail ys)