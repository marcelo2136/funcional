import Control.Monad

toInt :: [String] -> [Int]
toInt xs = [x1 xs, x2 xs]
    where x1 xs = read (xs !! 0) :: Int
          x2 xs = read (xs !! 1) :: Int

competidores [] = []
competidores xs = [toInt $ words (head xs)] ++ competidores (tail xs)

zipInd xs = zip[0..] xs

validation [] = []
validation xs = if  (snd (head xs)) !! 0 >= 10 && (snd (head xs)) !! 1 >= 10 then [head xs] ++ validation (tail xs) else validation (tail xs)

delta []    = []
delta (x:xs) = if (snd x) !! 0 > (snd x) !! 1 then [( fst x, ((snd x) !! 0) - ((snd x) !! 1))] ++ delta xs else [(fst x, ((snd x) !! 1) - ((snd x) !! 0))] ++ delta xs

winner xs = fst $ foldl (\(a, b) (c, d) -> if b <= d then (a,b) else (c,d)) (head  xs) $ tail xs
    where xs' xs = zip[1..]xs

winnerF [] = "sem ganhador"
winnerF xs  = show $ winner xs

main = do
    size <- readLn :: IO Int
    xs <- replicateM size getLine --(readLn :: IO String)
    print $ winnerF $ delta $ validation $ zipInd $ competidores xs