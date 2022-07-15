import Data.List
import Data.Maybe

letters = zip['a'..'z']['A'..'Z']

upper [] = []
upper (x:xs)
    | (elem x $ fst $ unzip letters) == True = [snd (unzip letters) !! (fromJust $ elemIndex x (fst (unzip letters)))] ++ upper xs
    | otherwise = [x] ++ upper xs
    



lower [] = []
lower (x:xs)
    | (elem x $ snd $ unzip letters) == True = [fst (unzip letters) !! (fromJust $ elemIndex x (snd (unzip letters)))] ++ lower xs
    | otherwise = [x] ++ lower xs

fixer (x:xs) = upper [x] ++ lower xs

titulo' [] = []
titulo' xs = [fixer (head xs)] ++ titulo' (tail xs)

titulo xs = unwords (titulo' (words xs))



isPalind [] = True
isPalind [x] = True
isPalind xs = if head xs == head (reverse (tail xs)) then isPalind (tail (reverse (tail xs))) else False


selec xs [] = []
selec xs (y:ys) = [xs !! y] ++ selec xs ys