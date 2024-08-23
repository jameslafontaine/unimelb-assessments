import DFA
import EqDFA -- hidden module, required for testing purposes

dfa :: DFA
dfa
-- put your definition here
  = ([1,2,3,4,5], "abcd", t, 1, [1,2,3,4])
  where
    t = [ ((1,'a'),2)
        , ((1,'d'),2)
        , ((1,'c'),3)
        , ((1,'b'),5)
        , ((2,'a'),2)
        , ((2,'d'),2)
        , ((2,'c'),3)
        , ((2,'b'),5)
        , ((3,'c'),3)
        , ((4,'b'),5)
        , ((4,'c'),3)
        , ((5,'b'),4)
        ]