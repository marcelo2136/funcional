mymap x [] = []
mymap x xs = [x (head xs)] ++ mymap x (tail xs)



myFilter x [] = []
myFilter x xs = if x (head xs) then [head xs] ++ myFilter x (tail xs) else myFilter x (tail xs)



mytails [x] = [[x],[]]
mytails xs  = [xs] ++ mytails (tail xs)



mynub' ys [] = ys
mynub' ys xs = if elem (head xs) ys == False && null xs == False then mynub' (ys ++ [head xs]) (tail xs) else mynub' ys (tail xs)
mynub xs = mynub' [head xs] (tail xs)