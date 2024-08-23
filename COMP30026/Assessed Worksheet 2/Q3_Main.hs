import Exp
import Data.Char -- needed for `ord`
import Data.List -- needed for tests

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
-- COPY OVER YOUR PREVIOUS SOLUTION
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



--
-- Tseytin transformation:
--

data TseytinTree
  = TTVar  Int
  | TTAnd  Int TseytinTree TseytinTree
  | TTOr   Int TseytinTree TseytinTree
  | TTXor  Int TseytinTree TseytinTree
  | TTBiim Int TseytinTree TseytinTree
  deriving (Eq, Show)

ttid :: TseytinTree -> Int
-- Find the id of the node at the root of a Tseytin Tree
ttid (TTVar  i)     = i
ttid (TTAnd  i _ _) = i
ttid (TTOr   i _ _) = i
ttid (TTXor  i _ _) = i
ttid (TTBiim i _ _) = i


tseytin :: Exp -> [[Int]]
-- Transform a Boolean expression into an equisatisfiable CNF formula
-- represented as a list of positive or negative variable numbers.

tseytin exp
  = [ttid tree] : transform tree
  where
    (tree, _) = maketree (prepare exp) 128
    
    maketree :: Exp -> Int -> (TseytinTree, Int)
    -- Transform a prepared Boolean expression into a Tseytin tree (with one
    -- new variable per connective). Start numbering new variables at the int
    -- provided, return the root variable's integer plus one.
    maketree (NOT (VAR x)) i
      = (TTVar (-(ord x)), i)
    maketree (VAR x) i
      = (TTVar (ord x), i)
    maketree (AND e f) i
      = (TTAnd k et ft, k+1)
      where
        (et, j) = maketree e i
        (ft, k) = maketree f j
    maketree (OR e f) i
      = (TTOr k et ft, k+1)
      where
        (et, j) = maketree e i
        (ft, k) = maketree f j
    maketree (XOR e f) i
      = (TTXor k et ft, k+1)
      where
        (et, j) = maketree e i
        (ft, k) = maketree f j
    maketree (BIIM e f) i
      = (TTBiim k et ft, k+1)
      where
        (et, j) = maketree e i
        (ft, k) = maketree f j

    transform :: TseytinTree -> [[Int]]
    -- transform a Tseytin Tree to a CNF Formula

    transform (TTVar i)
      = [] -- variables are captured by the clauses of their parent nodes

    transform (TTOr i e f)
      = [-i, ttid e, ttid f]
      : [i, -ttid e]
      : [i, -ttid f]
      : (transform e ++ transform f)
    -- FILL IN THE REMAINING TRANSFORMATIONS:
    transform (TTAnd i e f)
      = [i, -ttid e, -ttid f]
      : [-i, ttid e]
      : [-i, ttid f]
      : (transform e ++ transform f)
    transform (TTBiim i e f)
      = [i, -ttid e, -ttid f]
      : [-i, -ttid e, ttid f]
      : [-i, ttid e, -ttid f]
      : [i, ttid e, ttid f]
      : (transform e ++ transform f)
    transform (TTXor i e f)
      = [i, ttid e, -ttid f]
      : [i, -ttid e, ttid f]
      : [-i, -ttid e, -ttid f]
      : [-i, ttid e, ttid f]
      : (transform e ++ transform f)
