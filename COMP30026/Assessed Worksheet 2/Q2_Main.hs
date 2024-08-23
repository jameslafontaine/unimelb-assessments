import Exp

-- 
-- Preparing to transform
-- 

prepare :: Exp -> Exp
-- Prepare an arbitrary Boolean expression for Tseytin transformation
-- by pushing negations in and eliminating all implications.

-- cancel double negations as they occur
prepare (NOT (NOT e))
  = prepare e

-- push negations over AND, OR (De Morgan) and BIIM, XOR
prepare (NOT (OR e f))
  = AND (prepare (NOT e)) (prepare (NOT f))
-- COPY OVER YOUR PREVIOUS SOLUTION
prepare (NOT (AND e f))
  = OR (prepare (NOT e)) (prepare (NOT f))
prepare (NOT (BIIM e f))
  = XOR (prepare e) (prepare f) 
prepare (NOT (XOR e f))
  = BIIM (prepare e) (prepare f) 

-- otherwise, pass over these connectives
prepare (AND e f)
  = AND (prepare e) (prepare f)
prepare (OR e f)
  = OR (prepare e) (prepare f)
prepare (BIIM e f)
  = BIIM (prepare e) (prepare f)
prepare (XOR e f)
  = XOR (prepare e) (prepare f)

-- stop to unroll any implications encountered
-- (This will save us a few extra cases later!)
-- ADD CASES TO ELIMINATE IMPLICATION HERE
prepare (NOT (IMPL e f))
  = AND (prepare e) (prepare (NOT f))
prepare (IMPL e f)
  = OR (prepare (NOT e)) (prepare f)
  
-- stop once the negations get down to the variables
-- (variables and negated variables are OK)
prepare (VAR x)
  = VAR x
prepare (NOT (VAR x))
  = NOT (VAR x)
