elemento x xs = xs !! x2
    where x2 = if x < 0 then x + length xs else x

elemento1 0 ys = head ys
elemento1 y ys = elemento1 y2 (tail ys)
    where y2 = if y < 0 then (y-1) + length ys else y-1



pertence x xs = elem x xs

pertence2 x xs = if length filtering > 0 then True else False
    where filtering = filter (== x) xs

pertence3 x xs
    | head xs /= x = pertence3 x $ tail xs
    | null xs == True = False
    | otherwise = True



total xs = sum (map (1+) (map (0*) xs))

total2 [] = 0
total2 xs = 1 + (total2 $ tail xs)

total3 xs = foldl (\acc a -> if null xs == True then acc else acc + 1) 0 xs



maior [x] = x
maior xs = if head xs > head (tail xs) then maior ([head xs] ++ (rest $ tail xs)) else maior $ tail xs
    where rest xs = tail xs



primeiros xs = reverse $ drop 1 $ reverse xs



divide xs x = (take x xs, reverse (take (length xs - x) $ reverse xs))



splitints x xs = (filter x xs, filter (not . x) xs)



somaImpares xs = sum (filter odd xs)

somaImpares2 xs = sum ([x | x <- xs, x `mod` 2 /= 0])



max3 x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | z >= x && z >= y = z



fatorial 0 = 1
fatorial x = x * fatorial (x-1)



uniao xs ys = xs ++ filter (not . (`elem` xs)) ys



intersec xs ys = filter (`elem` xs) ys



sublist x y xs = drop x $ take y xs



paridade [] = False
paridade xs = if counter xs `mod` 2 /= 0 then True else False
    where counter xs = foldl (\acc a -> if a == True then acc+1 else acc) 0 xs



swap [] x y = []
swap [z] x y = [z]
swap xs x y = take x xs ++ [xs !! y] ++ drop (x + 1) (take y xs) ++ [xs !! x] ++ drop (y + 1) xs 



euler1 x = foldl (\acc a -> if a `mod` 5 == 0 || a `mod` 3 == 0 then acc + a else acc) 0 $ list x
    where list x = [1..x-1]