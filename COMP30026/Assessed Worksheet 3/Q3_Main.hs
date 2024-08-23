import DFA
import Data.Char (isDigit, isLower)
import Data.Maybe

-- -------------------------------------------------------------------
--
-- A DFA emulator written in Haskell.
--
-- -------------------------------------------------------------------

-- The result of running the DFA on some input is a Decision:

data Decision = Accept | Reject
  deriving (Eq, Show)


-- The DFA steps through successive configurations.  A configuration 
-- is a pair (state, remaining input).

type Configuration = (State, Input)


-- The function `runDFA' does the step-by-step simulation of its
-- argument dfa running on a particular input, and returns the
-- Decision (Accept or Reject) according to whether the DFA accepts
-- or rejects this input.
-- 
-- This function assumes that dfa is a well-formed DFA and that
-- input contains only symbols from the DFA's alphabet.

runDFA :: DFA -> Input -> Decision
runDFA (_, _, delta, startState, acceptStates) input
  = if endState `elem` acceptStates then Accept else Reject
    where
      -- Initially we need to explore the configuration
      -- (startState, input). From there, we should carry
      -- out transitions according to delta until the DFA's
      -- input is consumed entirely.
      (endState, _) = until finished (step delta) (startState, input)

-- The following helper functions make this process possible:

-- The computation should continue until it reaches a
-- configuration with no remaining input.

finished :: Configuration -> Bool
finished (_, remainingInput) = null remainingInput


-- We introduce a special 'dead state'/'reject state', to be used
-- when the given transition function hasn't been completed.
-- Since a well-formed DFA's states must be non-negative integers,
-- using -1 will suffice: Any future transitions will also be
-- missing from the transition function, and this state will not be
-- an accept state.

deadState :: State
deadState = -1

-- The function `step' completes a single step of the DFA computation.
-- It takes as input the transition function of the DFA, and the
-- current configuration (current state, remaining input). It computes
-- the next configuration according to the current state, the next
-- symbol in the input, and the transition function.
-- 
-- If the transition function does not contain a transition for the
-- current configuration, we transition to the special non-accepting 
-- 'dead state' which will lead to all remaining symbols being
-- consumed, and the input being rejected.

step :: [Transn] -> Configuration -> Configuration
-- PUT YOUR DEFINITION HERE
step transn (state, x:xs)
  = ((fromMaybe deadState (lookup (state, x) transn)), xs) 




-- -------------------------------------------------------------------
--
-- Functions for interactive mode.
-- 

-- Usage: 
-- 
-- `use d` where d is a well-formed DFA, that is, a tuple
--
--        (states, alphabet, delta, startState, acceptStates)

-- We rely on module DFA to provide definitions of types as well as
-- tools for checking the well-formedness of DFAs.
-- 
-- The action `use d' will enter an interactive mode where it will
-- repeatedly query for input to d, run d on the input, print Accept
-- or Reject, as appropriate, and query for new input.  This makes
-- it easy to try the same DFA on different input.  The function
-- will exit the read-eval loop when the user types `Quit'.

-- -------------------------------------------------------------------

-- Definitions (for interest only).

-- The function `use' takes a DFA d and enters a dialogue with the
-- user, repeatedly asking for input to d, and printing d's decision.

use :: DFA -> IO ()
use d
  = do
      putStr "Input: "
      input <- getLine
      if compliant input
        then do {putStrLn (show (runDFA d input)); use d}
        else if (head input) == 'Q'
          then return ()
          else do 
            putStrLn "Input must be digits and/or lower case letters"
            use d

-- The function `compliant' checks that input consists entirely of
-- digits and/or lower case letters.

compliant :: Input -> Bool
compliant
  = all comply
    where
      comply c = isLower c || isDigit c
