import Exp

-- ----------------------------------------------------------------------
--
-- Wang's algorithm for validity checking  (incomplete)
--

wang ::    [Exp]   -- Left-hand side of (unreduced) sequent
        -> [Exp]   -- Right-hand side of (unreduced) sequent
        -> [Char]  -- Accumulator for fully reduced part of left-hand side
        -> [Char]  -- Accumulator for fully reduced part of right-hand side
        -> Bool    -- Output indicates whether the input formula was valid

-- Recursive cases:

-- A conjunction of two formulas on the LHS or a disjunction of two
-- formulas on the RHS can just be split up into its component formulas.
wang (AND f g : left) right reducedLeft reducedRight
  = wang (f : g : left) right reducedLeft reducedRight
wang left (OR f g : right) reducedLeft reducedRight
  = wang left (f : g : right) reducedLeft reducedRight

-- However, if there's a disjunction on the LHS or a conjunction on the
-- RHS then we need to split the sequent in two. The original sequent
-- is valid if and only if both of the new sequents are.
wang (OR f g : left) right reducedLeft reducedRight
  = wang (f : left) right reducedLeft reducedRight &&
    wang (g : left) right reducedLeft reducedRight
wang left (AND f g : right) reducedLeft reducedRight
  = wang left (f : right) reducedLeft reducedRight &&
    wang left (g : right) reducedLeft reducedRight

-- We can reduce a negated formula on either side by removing the
-- negation and moving the formula to the other side of the sequent.
wang (NOT f : left) right reducedLeft reducedRight
  = wang left (f : right) reducedLeft reducedRight
wang left (NOT f : right) reducedLeft reducedRight
  = wang (f : left) right reducedLeft reducedRight

-- When we see a propositional variable on either side, this part of
-- the LHS or RHS has been fully reduced. Note: there's no need to
-- store these as expressions: we just store the letters themselves.
wang (VAR x : left) right reducedLeft reducedRight
  = wang left right (x : reducedLeft) reducedRight
wang left (VAR x : right) reducedLeft reducedRight
  = wang left right reducedLeft (x : reducedRight)


-- Base case:

-- Eventually, there will be no non-reduced parts remaining on either
-- the LHS or RHS of the sequent. In this case, the sequent is valid
-- if and only if some propositional letter occurs on both
-- `reducedLeft` and `reducedRight`.
wang [] [] reducedLeft reducedRight
  = or [ elem lit reducedRight | lit <- reducedLeft]      -- FIX THIS                                         


-- Initial case:

valid :: Exp -> Bool
-- The algorithm starts by converting the input formula to a sequent
-- with the formula on its right hand side.
valid f
  = wang [] [f] [] []
