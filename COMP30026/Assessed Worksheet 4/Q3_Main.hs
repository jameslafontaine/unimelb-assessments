import Data.List (nub, sort) -- add more imports if required
import DFA
import NFA
import VisDFA
import Hidden -- required for testing purposes


determiniseNFA :: NFA -> DFA
determiniseNFA nfa
  = renumberDFA $ trimDFA (qs', xs, ts', q0', as')
  where
    (qs, xs, ts, q0s, as) = reorderNFA $ removeEpsilonNFA nfa

    q0' = q0s
    qs' = powerlist qs
    as' = filter (any (`elem` as)) qs'
    ts' = [((rs, x), step rs x) | rs <- qs', x <- xs]

    step :: [Int] -> Symbol -> [Int]
    step rs x
      = nub $ sort $ concatMap (`step1` x) rs

    -- TODO: Complete definition for step1 r x
    step1 :: Int -> Symbol -> [Int]
    step1 r x
      = []