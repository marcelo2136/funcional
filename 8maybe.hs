import Data.List
import Data.Maybe

myElemIndex x xs = elemIndex x xs

somaMaybe x y
    | isJust x && isJust y      = Just (fromJust x + fromJust y)
    | isJust x && isNothing y   = Just $ fromJust x
    | isNothing x && isJust y   = Just $fromJust y
    | otherwise                 = Nothing

filterMaybe [] = []
filterMaybe (x:xs) = if isJust x == True then [fromJust x] ++ filterMaybe xs else filterMaybe xs