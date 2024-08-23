module Formula
where

import Data.Char
import Data.List

-------------------------------------------------------------------------
--
-- The following types are used to represent propositional formulas in
-- conjunctive normal form.
--
-- In conjunctive normal form, a Formula is a conjunction of Clauses.
-- A Clause is a disjunction of Literals. A Literal is a propositional
-- variable, or its negation.
--

data Literal
  = Pos Char | Neg Char
    deriving (Eq, Ord, Show)

type Clause
  = [Literal]

type Formula
  = [Clause]


-------------------------------------------------------------------------
--
-- The following functions are helpful for manipulating formulas.
--

negate_literal :: Literal -> Literal
-- Given a literal, switch its sign. That is, turn a propositional variable
-- into its negation, or remove the negation from a negated variable.
negate_literal (Pos c) 
  = Neg c
negate_literal (Neg c) 
  = Pos c

negate_formula :: Formula -> Formula
-- Given a set f of clauses, return a set of clauses representing ~f.
negate_formula
  = reduce . remove_tautologies . map switch_signs . to_dnf
    where 
      switch_signs = reduce . map negate_literal

      to_dnf :: Formula -> Formula
      -- Convert a CNF formula to DNF (negation will produce CNF again).
      to_dnf []
        = [[]]
      to_dnf (c:cs)
        = distribute c (to_dnf cs)
          where
            distribute [] _ = []
            distribute (lit:lits) dnf = map (lit:) dnf ++ distribute lits dnf


reduce :: Ord a => [a] -> [a]
-- Sort and remove duplicates from a list.
reduce 
  = nodups . sort
    where
      nodups (x:y:ys) 
        | x == y    = xs
        | otherwise = x:xs
          where xs  = nodups (y:ys)
      nodups xs
        = xs
      
normalise :: Formula -> Formula
-- Arrange a formula's clauses in order of increasing length.
normalise cs
  = map snd (sort annotated_clauses)
    where 
      length_annotate c = (length c, c)
      annotated_clauses  = map length_annotate cs

remove_tautologies :: Formula -> Formula
-- Remove tautological clauses from a formula (these do not change the formula).
remove_tautologies cs
  = filter (not . tautology) cs
    where
      tautology :: Clause -> Bool
      -- A clause is a tautology if any of its literals appears in both negated 
      -- and non-negated form.
      tautology []
        = False
      tautology (lit:lits)
        = (negate_literal lit `elem` lits) || (tautology lits)


-------------------------------------------------------------------------
--
-- The function translate supports the use of short strings as a way of
-- writing lists of clauses using types Literal, Clause and Formula.
-- In the string representation, `$' signifies the start of a clause,
-- and `-' stands for negation.  For example,
--
--                         "$A-BC$-A-D$BCD" 
-- translates to
--
-- [[Pos 'A',Neg 'B',Pos 'C'],[Neg 'A',Neg 'D'],[Pos 'B',Pos 'C',Pos 'D']]
--
-- With that convention, "" denotes the empty formula (i.e., true) and
-- "$" denotes the formula consisting of an empty clause (i.e., false).
--

translate :: String -> Formula
translate s
  = tr (reverse s) [] []
    where
      tr :: String -> Clause -> Formula -> Formula
      tr [] _ clause_stack
        = clause_stack
      tr ('$':s) literal_stack clause_stack
        = tr s [] (literal_stack : clause_stack)
      tr ('-':s) (Pos c : literal_stack) clause_stack
        = tr s (Neg c : literal_stack) clause_stack
      tr (c:s) literal_stack clause_stack
        | isUpper c = tr s (Pos c : literal_stack) clause_stack
        | otherwise = error ("Illegal input character: " ++ [c])
