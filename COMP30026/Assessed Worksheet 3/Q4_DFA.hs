module DFA (
  DFA
, State
, Symbol
, Transn
, Input
, checkDFA
)
where

import Data.List (nub, sort, (\\))
import System.Exit

-- -------------------------------------------------------------------
-- 
-- A type definition for DFAs, and a function that checks
-- well-formedness of DFAs.
--
-- -------------------------------------------------------------------

type State  = Int
type Symbol = Char
type Transn = ((State, Symbol), State)
type DFA    = ([State], [Symbol], [Transn], State, [State])
type Input  = [Symbol]


-- Check a DFA for well-formedness, exiting with an error if the
-- input DFA is not well-formed in some way.

checkDFA :: DFA -> IO ()
checkDFA (states, alphabet, delta, start, accept) 
  = do
      let nondets = nonDets delta
      let usedStates = start : (accept ++ statesOf delta)
      let sortedStates = nub (sort states)
      let sortedUsedStates = nub (sort usedStates)
      let nonDeclaredStates = sortedUsedStates \\ sortedStates
      checkValid states
      checkDeclared nonDeclaredStates
      checkNondets nondets
      checkSymbols (symbols delta) alphabet
      putStrLn "SUCCESS!"

-- -------------------------------------------------------------------
--  
-- Helper functions.
--
-- -------------------------------------------------------------------


statesOf :: [Transn] -> [State]
statesOf delta
  = concatMap statesOfTransn delta
    where
      statesOfTransn ((st,_),st') = [st,st']

nonDets :: [Transn] -> [(State,Symbol)]
nonDets delta
  = domain \\ nub domain
    where
      domain = map fst delta

symbols :: [Transn] -> [Symbol]
symbols delta
  = nub (sort syms)
    where
      domain = map fst delta
      syms = map snd domain


checkValid :: [State] -> IO ()
checkValid states
  = do
      let illegalStates = filter (<0) states
      if null illegalStates then
        return ()
      else
        do
          putStrLn "ERROR:"
          putStrLn ("Illegal states: " ++ show illegalStates)
          exitWith (ExitFailure 1)

checkDeclared :: [State] -> IO ()
checkDeclared states
  = if null states then
      return ()
    else
      do
        putStrLn "ERROR:"
        putStrLn ("States used but not declared: " ++ show states)
        exitWith (ExitFailure 1)

checkNondets :: [(State,Symbol)] -> IO ()
checkNondets entries
  = if null entries then
      return ()
    else
      do
        putStrLn "ERROR:"
        putStrLn ("Non-deterministic entries: " ++ show entries)
        exitWith (ExitFailure 1)

checkSymbols :: [Symbol] -> [Symbol] -> IO ()
checkSymbols syms alphabet
  = do
      let badSyms = syms \\ alphabet
      if null badSyms then
        return ()
      else
        do
          putStrLn "ERROR:"
          putStr ("Symbol used, not in alphabet: ")
          putStrLn (show (head badSyms))
          exitWith (ExitFailure 1)
