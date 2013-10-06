module Utils where

import Data.Maybe

(.:) = (.) . (.)


move :: [[Maybe a]] -> (Int, Int) -> (Int, Int) -> [[Maybe a]]
move board (x, y) (x2, y2) = fullMat
  where
    elem = (board !! y) !! x
    fullMat = replace y2 fullRow emptyMat
    fullRow = replace x2 elem (emptyMat !! y2)
    emptyMat = replace y emptyRow board
    emptyRow = replace x Nothing (board !! y)


replace :: Int -> a -> [a] -> [a]
replace i x xs = left ++ (x : (tail right))
  where
    (left, right) = splitAt i xs


get :: [[a]] -> (Int, Int) -> a
get xss (c, r)= (xss !! r) !! c


checkCoords :: (Int, Int) -> Maybe (Int, Int)
checkCoords (c, r)
  | c >= 0 && c < 8 && r >= 0 && r < 8 = Just (c, r)
  | otherwise = Nothing


removeNothing :: [Maybe a] -> [a]
removeNothing = map fromJust . filter isJust
