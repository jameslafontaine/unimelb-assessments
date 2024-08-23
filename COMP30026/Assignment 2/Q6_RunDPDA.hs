module RunDPDA
where
import Data.Char
import DPDA

----------------------------------------------------------------------------
--  A DPDA simulator written in Haskell
----------------------------------------------------------------------------

--  Usage: 'use p' where p is a DPDA. The function will ask for
--  input to p, then print Accept or Reject, as appropriate.
--  Or, to run p on a single input, use 'runDPDA p input'

data Decision
  = Accept | Reject
    deriving (Eq, Show)

--  There is an assumption that the DPDA is well-formed and deterministic.
--  Each transition step consumes an input symbol. Symbols are character,
--  but for technical reasons, the input alphabet is assumed to contain
--  only digits and lower case letters. We follow the convention of using
--  'eps' in stack operations to mean 'nothing'. That is, 'pop x' is
--  captured as 'replace x by eps', 'push x' as 'replace eps by x',
--  and 'leave the stack untouched' by 'replace eps by eps'.

--  A configuration is a triple (state, stack, remaining input).

type Configuration = (State, Stack, [Symbol])

--  The function `runDPDA' does the step-by-step simulation.

runDPDA :: DPDA -> Input -> Decision

runDPDA (_, _, _, delta, startState, acceptStates) input
  = if compliant input
    then
      if endState `elem` acceptStates then Accept else Reject
    else
      error "DPDA input should be in [a-z0-9]*: lower case letters/digits"
    where
      (endState, _, _)
        = until done (step delta) (startState, [], input)

-- The computation should continue until it reaches a configuration
-- with no remaining input.

done :: Configuration -> Bool
done (_, _, remainingInput)
  = null remainingInput

-- We introduce a special 'dead state'/'reject state', to be used
-- when the given transition function hasn't been completed.
-- Since a well-formed DPDA's states must be non-negative integers,
-- using -1 will do the trick.

deadState :: State
deadState = -1

step :: [Transn] -> Configuration -> Configuration

step delta (state, [], nextSymbol : remainingInput)
  = case lookup (state, nextSymbol, eps) delta of
      Just (newState, newTop)
        -> if newTop == eps then
             (newState, [], remainingInput)
           else
             (newState, [newTop], remainingInput)
      Nothing
        -> (deadState, [], "")

step delta (state, stack, nextSymbol : remainingInput)
  = case lookup (state, nextSymbol, top) delta of
      Just (newState, newTop)
        -> if newTop == eps then
             (newState, stackTail, remainingInput)
           else
             (newState, newTop : stackTail, remainingInput)

      Nothing
        -> case lookup (state, nextSymbol, eps) delta of
             Just (newState, newTop)
               -> if newTop == eps then
                    (newState, stack, remainingInput)
                  else
                    (newState, newTop : stack, remainingInput)
             Nothing
               -> (deadState, stack, "")
    where
      (top : stackTail) = stack

--  The function `use' takes a PDA p and enters a dialogue with the
--  user, asking for input to p, and printing p's decision.

use :: DPDA -> IO ()

use p
  = do
      putStr "Input (Q to quit): "
      input <- getLine
      if compliant input
        then do {putStrLn (show (runDPDA p input)); use p}
        else if (head input) == 'Q'
          then return ()
          else do
            putStrLn "Input must be digits and/or lower case letters"
            use p

--  The function `compliant' checks that input consists entirely of digits
--  and lower case letters.

compliant :: Input -> Bool
compliant
  = all comply
    where
      comply c = isLower c || isDigit c