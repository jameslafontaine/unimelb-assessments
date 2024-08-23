module Main
    ( module Properties
    , module Sorts
    , sortSpec 
    ) where
    
import Properties
import Sorts
import Data.List (permutations)
    
sortSpec :: (Ord a) => ([a] -> [a]) -> [a] -> Bool
sortSpec sort input = 
    elem (sort input) (permutations input) && sortIsSorted sort input