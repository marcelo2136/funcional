import Data.List

fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)



frequencia x xs = length (filter (==x) xs)

frequencia2 x xs = length ([a | a <- xs, a == x])

frequencia3 x [] = 0
frequencia3 x xs = if x /= head xs then frequencia3 x $ tail xs else frequencia3 x (tail xs) + 1

frequencia4 x xs = foldl (\acc a -> if a == x then acc+1 else acc) 0 xs



unico x xs = if length (filter (==x) xs) == 1 then True else False

unico2 x [] = False
unico2 x [y] = if x == y then True else False
unico2 x xs =  if x == head xs && unico2 x (tail xs) == False then True else False



maioresQue x xs = filter (>x) xs


concatena [] [] = []
concatena xs [] = xs
concatena [] ys = ys
concatena xs ys = if null xs == False then head xs : concatena (tail xs) ys else head ys : concatena xs (tail ys)


alter 0 = []
alter x = reverse $ alter' x
    where alter' x = if x > 0 then [-x] ++ [x] ++ alter' (x-1) else []



reverso [x] = [x]
reverso xs = reverso (tail xs) ++ [head xs]


menorLista [x] = x
menorLista xs = if head xs > head (tail xs) then menorLista (tail xs) else menorLista ([head xs] ++ tail (tail xs))

menores' 0 xs = []
menores' y ys = [menorLista ys] ++ menores' (y-1) (menorFora 0 ys)
    where menorFora x xs = if (menorLista xs == xs !! x) && x < length xs then (take x xs) ++ (drop (x+1) xs) else menorFora (x+1) xs

menores x xs = filter (`elem` (menores' x xs)) xs



maior [x] = x
maior xs = if head xs < head (tail xs) then maior (tail xs) else maior ([head xs] ++ tail (tail xs))

removerMaior [x] = []
removerMaior xs = filter (< (maior xs)) xs



intercal [] [] = []
intercal xs [] = xs
intercal [] ys = ys
intercal xs ys = [head xs] ++ [head ys] ++ intercal (tail xs) (tail ys)

intercal2 xs ys
    | length xs > length ys = reverse $ foldl (\list (a,b) -> if b == -1 then [a] ++ list else [b] ++ [a] ++ list) [] $ zip xs (ys ++ [(-1),(-1)..])
    | length ys > length xs = reverse $ foldl (\list (a,b) -> if a == -1 then [b] ++ list else [b] ++ [a] ++ list) [] $ zip (xs ++ [(-1),(-1)..]) ys
    | otherwise             = reverse $ foldl (\list (a,b) -> [b] ++ [a] ++ list) [] $ zip xs ys



sequencia 0 y = []
sequencia x y = [y] ++ sequencia (x-1) (y+1)



rotEsq 0 xs = xs
rotEsq x xs = drop x xs ++ take x xs



rotDir 0 xs = xs
rotDir x xs = drop ((length xs) - x) xs ++ take ((length xs) - x) xs


raiz (-1) y = False
raiz x y = if x*x == y then True else raiz (x-1) y

quadperf x = if x >= 0 && raiz x x == True then True else False



deletee x [] = []
deletee x xs = if x == head xs then tail xs else [head xs] ++ deletee x (tail xs)


separa :: Int -> [Int]
separa x = if x < 10 then [x] else separa (x `div` 10) ++ [x `mod` 10]

separa2 0 = [0]
separa2 x = unfoldr (\b -> if b == 0 then Nothing else Just (b `mod` 10, b `div` 10)) x



