-- Please don't remove or alter these imports, 
-- as they are required for marking.
module Properties
    ( sortLength
    , sortHead
    , sortIsSorted
    , example
    , counterExample
    ) where
-------------------------------------------------

import Sorts

--------------------------------------------------------------------------------
-- The Tasks
-------------

-- Task 4A
sortLength :: (Ord a) => ([a] -> [a]) -> [a] -> Bool
sortLength sort input = length (sort input) == length input

-- Task 4B
sortHead :: (Ord a) => ([a] -> [a]) -> [a] -> Bool
sortHead sort input = null input || head (sort input) == minimum input

-- Task 4Cs
sortIsSorted :: (Ord a) => ([a] -> [a]) -> [a] -> Bool

sortIsSorted sort [] = True
sortIsSorted sort [a] = True

sortIsSorted sort (x:xs) = sort (x:xs) !! 0 <= sort (x:xs) !! 1 && sortIsSorted sort xs

-- Task 4D
example :: [Int]
example = [3,2,1]

counterExample :: [Int]
counterExample = [3,3,2,1]