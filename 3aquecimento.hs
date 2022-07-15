-- AQUECIMENTO
countNeg xs = length(filter (< 0) xs)

final x xs = drop (length xs - x) xs

iguais x y z = if x == y && y == z then 3 else (doisIguais x y) + (doisIguais x z) + (doisIguais y z)
    where doisIguais f g = if f == g then 2 else 0

interior xs = drop 1 (take (length xs - 1) xs)

gangorra x1 x2 y1 y2
    | x1*x2 > y1*y2 = -1
    | x1*x2 < y1*y2 = 1
    | otherwise = 0

min2 x y = if x >= y then y else x

min3 x y z
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | z <= x && z <= y = z

soma x y = x + y