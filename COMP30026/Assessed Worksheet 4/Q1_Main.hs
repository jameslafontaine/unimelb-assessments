import Data.List (union, intersect) -- add more imports if required
import DFA
import VisDFA
import Hidden -- required for testing purposes


productDFA :: DFA -> DFA -> DFA
productDFA (qs, xs, ts, q0, as) (qs', xs', ts', q0', as')
  = renumberDFA $ trimDFA (qs'', xs'', ts'', q0'', as'')
  where
    qs'' = crosslist qs qs'
    xs'' = union xs xs'
    q0'' = (q0, q0')
    as'' = crosslist as as'

    -- TODO: Complete definition for ts''
    ts'' :: [GTransn (Int, Int)]
    ts'' = []