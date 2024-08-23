-- we provide the same files as in assessed worksheet 4
import DFA
import NFA
import VisDFA
-- we provide solutions to worksheet 4 problems, you may find useful
import HiddenAlgorithms (determiniseNFA, minimiseDFA)
-- one more module is required for testing purposes
import Hidden
import Data.Maybe

-- TODO: Complete this function
slice :: DFA -> DFA
slice dfa
  = determiniseNFA (states, alphabetDFA dfa, transns, starts, accepts)
    where
    numStates = (length (statesDFA dfa))
    states = (statesDFA dfa) ++ (map (+numStates) (statesDFA dfa))
    transns = [ ((stateFrom + numStates, eps), stateTo + numStates) 
                | ((stateFrom, char), stateTo) <- transnsDFA dfa ] 
              ++
              [ ((stateFrom + numStates, char), stateTo) 
              | ((stateFrom, char), stateTo) <- transnsDFA dfa ]
              ++
              [ ((stateFrom, char), stateTo + numStates) 
              | ((stateFrom, char), stateTo) <- transnsDFA dfa ]
    starts = [startStateDFA dfa] -- ++ [startStateDFA dfa + numStates]
    paths = concat [  fromJust (path (edges (transnsDFA dfa)) (startStateDFA dfa) accState) 
                     | accState <- (acceptStatesDFA dfa)
                     , length (successors (edges (transnsDFA dfa)) accState) == 0
                     ]
    subtractedAccepts = [ (a-1)| a <- paths, elem a (acceptStatesDFA dfa) ]                 
    accepts = -- subtractedAccepts ++ (map (+numStates) subtractedAccepts)
    -- acceptStatesDFA dfa ++ (map (+numStates) (acceptStatesDFA dfa))
    
    
everySnd :: [Int] -> [Int]
everySnd [] = []
everySnd [x] = []
everySnd (x:y:ys) = (y :(everySnd ys))
    
dfa0 :: DFA
dfa0 = ([1,2,3,4,5], "abcd", [((1, 'a'), 2), ((1, 'b'), 5), ((2, 'b'), 3), ((3, 'c'), 4), ((4, 'd'), 5)] , 1, [2,3,5]) 
dfa1 :: DFA
dfa1 = ([1,2,3,4], "abc", [((1,'a'),2),((1,'b'),3),((1,'c'),4),((2,'c'),4),((3,'b'),2),((4,'a'),3)],1,[4])
dfa2 :: DFA
dfa2 = ([1,2,3,4,5], "abcd", [((1, 'a'), 2), ((1, 'b'), 5), ((2, 'b'), 3), ((3, 'c'), 4), ((4, 'd'), 5),((5,'c'),1)] , 1, [2,3,5])
dfa3 :: DFA
dfa3 = ([1,2,3,4], "abc", [((1,'a'),2), ((2,'b'),3), ((3,'c'),1),((3,'b'),4),((4,'a'),2)] , 1, [1,2])
dfa4 :: DFA
dfa4 = ([1,2,3,4,5,6], "abcd", [((1, 'a'), 2), ((1, 'b'), 5), ((2, 'b'), 3), ((3, 'c'), 4), ((4, 'd'), 5),((5,'a'),6),((3,'a'),1),((4,'a'),2)] , 1, [2,3,5,6])
dfa5 :: DFA
dfa5 = ([1,2,3,4,5], "abcde", [((1, 'a'), 2), ((2, 'b'), 3), ((3, 'c'), 4), ((4, 'd'), 5),((4,'a'),2),((5,'e'),1)] , 1, [1])
dfa6 :: DFA
dfa6 = ([1,2,3,4], "abc", [((1,'a'),2), ((2,'b'),3),((2,'a'),2), ((3,'c'),3),((3,'b'),4),((4,'a'),2)] , 1, [1,2])
dfa7 :: DFA
dfa7 = ([1,2,3,4], "abc", [((1,'a'),2), ((2,'b'),3),((2,'a'),2), ((3,'c'),3),((3,'b'),4),((4,'a'),2),((4,'b'),1)] , 1, [2])
dfa8 :: DFA
dfa8 = ([0,1,2,3],"ab",[((0,'a'),3),((0,'b'),0),((1,'a'),2),((1,'b'),1),((2,'b'),0),((3,'b'),1)],0,[0,2])
dfa9 :: DFA
dfa9 = ([1,2,3,4,5,6,7],"abcd",[((1,'a'),2),((1,'b'),6),((2,'b'),3),((3,'c'),4),((4,'d'),5),((6,'c'),7)],1,[2,3,5,7])