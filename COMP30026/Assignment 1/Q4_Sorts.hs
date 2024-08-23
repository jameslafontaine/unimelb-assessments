module Sorts
   ( msort
   , qsort
   ) where

msort :: (Ord a) => [a] -> [a]
msort xs@(_:_:_) = msort (take n xs) `merge` msort (drop n xs)
  where n = length xs `div` 2
        merge [] rs = rs
        merge ls [] = ls
        merge lls@(l:ls) rrs@(r:rs) 
            | l < r     = l:merge ls  rrs
            | otherwise = r:merge lls rs
msort xs = xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (pivot:rest) = qsort lesser ++ [pivot] ++ qsort greater
  where lesser  = filter (< pivot) rest
        greater = filter (> pivot) rest