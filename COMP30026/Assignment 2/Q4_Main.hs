import DFA
import NFA
import VisDFA
import Hidden
import Data.List
import Data.Maybe

sigma = "abcd"

findFixed :: DFA 
findFixed = ([1,2,3,4], sigma, delta, 1, [4])
            where delta = [ ((1, 'a'), 2)
                          , ((1, 'b'), 3)
                          , ((1, 'c'), 1)
                          , ((1, 'd'), 1)
                          , ((2, 'a'), 2)
                          , ((2, 'b'), 4)
                          , ((2, 'c'), 1)
                          , ((2, 'd'), 1)
                          , ((3, 'a'), 1)
                          , ((3, 'b'), 3)
                          , ((3, 'c'), 4)
                          , ((3, 'd'), 1)
                          , ((4, 'a'), 4)
                          , ((4, 'b'), 4)
                          , ((4, 'c'), 4)
                          , ((4, 'd'), 4) ]
                          
findDict :: [String] -> NFA
findDict dict 
    -- all inputs have the empty string as a substring so create a
    -- suitable NFA for dicts which contain the empty string
    | elem "" dict = ([1], sigma, delta, [1], [1])
    | otherwise = renumberNFA (unionNFAs ([ constructSingleNFA (rDict!!0) ] 
                             ++
                             [ constructSingleNFA (rDict!!x) 
                             | x <- [1..(length rDict) - 1] ] ))
    where rDict = reduceDict dict
          delta = [ ((1, 'a'), 1)
                  , ((1, 'b'), 1)
                  , ((1, 'c'), 1)
                  , ((1, 'd'), 1) ]
                        
constructSingleNFA :: String -> NFA
constructSingleNFA substring
    = (states, sigma, transns, starts, accepts) 
    where states = [0] ++ [1] ++ [ transformState i substring | i <- [0..((length substring)- 2)] ] 
          firstState = states!!2
                    -- first letter transitions from initial state to first sub state of NFA
          transns = [ ((0, char), firstState) 
                    | char <- sigma
                    , substring!!0 == char ]
                    ++
                    -- infinitely loop on first sub state if matching initial substring letter (unless first 2 letters of substring are duplicates)
                    [ ((firstState, char), firstState) 
                    | char <- sigma
                    , (length substring > 1) && substring!!0 == char && substring!!1 /= char ]
                    ++
                    -- progressive substring matches (closer to matching this substring)
                    [ (((transformState i substring), char), (transformState (i+1) substring)) 
                    | i <- [0..((length substring)- 3)]
                    , char <- sigma
                    , substring!!(i+1) == char ]
                    ++
                    -- final letter substring match
                    [ (((transformState ((length states) - 3) substring), char), 1) 
                    | char <- sigma
                    , last substring == char ]
                    ++
                    -- return to start state 0 if substring match has failed
                    [ (((transformState i substring), char), 0) 
                    | i <- [0..((length substring)- 2)]
                    , char <- sigma
                    , substring!!(i+1) /= char
                    , not (substring!!0 == char && i==0) ]
                    ++ 
                    -- once a complete substring match is found we infinitely loop in the accept state
                    [ ((1, char), 1) 
                    | char <- sigma ]
          starts = [0]
          accepts = [1]
    
-- combine the NFA definitions    
unionNFAs :: [NFA] -> NFA
unionNFAs nfas 
    = (states, sigma, nub transns, starts, accepts)
      where states = nub (concat [ statesNFA nfa | nfa <- nfas ])
            transns = (concat [ transnsNFA nfa | nfa <- nfas ])
                      ++ 
                      [ ((0, char), 0) | char <- sigma ]
            starts = [0]
            accepts = [1]
        
        
-- remove strings from the dict which contain other strings as substrings    
reduceDict :: [String] -> [String]
reduceDict dict = [ s 
                  | s <- dict
                  , not (any (\x -> x `isInfixOf` s) (delete s (nub dict)))]        


-- name states based on the letter and its position in the substring for consistency amongst sub NFAs
transformState :: Int -> String -> Int
transformState 0 substring = charToNum (substring!!0) + 1
transformState num substring = ((transformState (num-1) substring) * 4) + charToNum (substring!!num) + 1

charToNum :: Char -> Int
charToNum letter
    | letter == 'a' = 1
    | letter == 'b' = 2
    | letter == 'c' = 3
    | letter == 'd' = 4
    | otherwise = -1
    
{-                  
nextStartState :: [String] -> Int
nextStartState [] = 2
nextStartState (y:ys) = length (y) + nextStartState ys + 1
-}

{-constructSingleNFA :: String -> Int -> NFA
constructSingleNFA substring start
    = (states, alphabet, (nub transns), starts, accepts) 
    where states = [start..(start + (length substring))]
          alphabet = "abcd"
          transns = -- progressive substring matches (closer to matching this substring)
                    [ ((state, char), state+1) 
                    | state <- init states
                    , char <- alphabet
                    , substring!!(state - start) == char ]
                    ++ 
                    -- repeated letters on first letter of substring (substring not ruled out yet) (may have to account for same 
                    --                                                                              1st and 2nd letter case)
                    [ ((start + 1, char), start + 1) 
                    | char <- alphabet
                    , substring!!0  == char ]
                    ++
                    -- inconsequential start state letters (i.e. letters that aren't the beginning of this substring)
                    [ ((start, char), start) 
                    | char <- alphabet
                    , substring!!0 /= char ]
                    ++
                    -- failed substring matches (reset to start state (except for first state loop)
                    [ ((state, char), start) 
                    | state <- (tail states)
                    , char <- alphabet
                    , state < last states
                    , not (substring!!(state - start)  == char)
                    , not (substring!!0 == char && state == start + 1) ]
                    ++ 
                    -- successful substring matches (infinitely loop accept state)
                    [ ((last states, char), last states) 
                    | char <- alphabet ]
          starts = [start]
          accepts = [last states]
-}  

{-    -- combine the NFA definitions    
unionNFAs :: [NFA] -> NFA
unionNFAs nfas 
    = (states, sigma, nub transns, starts, accepts) -- re-add nub to starts and accepts if going with old solution
      where states = nub (concat [ statesNFA nfa | nfa <- nfas ])
            transns = (concat [ transnsNFA nfa | nfa <- nfas ])
                      ++ 
                      [ ((0, char), 0) | char <- sigma ]
            starts = [0]-- [ length states ]
            accepts = [1]-- concat [ acceptStatesNFA nfa | nfa <- nfas ]
-}

--readd (2 & nextStartState (take x rDict) as start arguments if old solution for findDict list comprehension)