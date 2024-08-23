import Data.List
import Formula

-- Add the function `entails` here:

entails :: Formula -> Formula -> Bool
-- Takes two Formulas f and g and returns True if and only if 
-- formula g is a logical consequence of f (in which case, 
-- we would say that f entails g).
entails f g = unsatisfiable (f ++ negate_formula g)

-------------------------------------------------------------------------

unsatisfiable :: Formula -> Bool
-- Keep evolving new generations and let them take part in the
-- resolution process.  The process is finished when no new 
-- resolvents can be found, or when the empty clause is produced.
unsatisfiable f
  = not (null latest)
    where
      f' = reduce (remove_tautologies (map reduce f))
      (older, latest) = until finished evolve ([], f')
      finished (old, young) = null young || [] `elem` young

evolve :: (Formula, Formula) -> (Formula, Formula)
-- Given a collection `(old, young)' of clauses (already closed under
-- resolution of those in `old' and with `young' being the recently
-- produced resolvents of `old'), resolve clauses in `young' against
-- all clauses in the collection to produce a new generation of clauses.
-- Also promote the current `young' clauses to the status of `old'.
evolve (old, young) 
  = (old_and_young, new_young \\ old_and_young)
    where
      old_and_young = reduce (old ++ young)
      new_young = reduce (remove_tautologies offspring)
      offspring = (offspring_of_both ++ offspring_of_young)
      offspring_of_both  = concat [resolvents c1 c2 | c1 <- old,   c2 <- young]
      offspring_of_young = concat [resolvents c1 c2 | c1 <- young, c2 <- young]

-- Copy in your previous solutions here:

resolvents :: Clause -> Clause -> [Clause]
-- Find all possible resolvents of two clauses.
resolvents c1 c2
  = [ resolve_on l c1 c2 | l <- c1 , elem (negate_literal l) c2]

resolve_on :: Literal -> Clause -> Clause -> Clause
-- resolve_on lit c1 c2 assumes that lit is known to be in c1
-- and its negation is known to be in c2.  It resolves on lit
-- and sorts and removes any duplicates in the resolvent.
resolve_on lit c1 c2
  = reduce ([ l | l <- c1 , l /= lit] 
           ++ [ l | l <- c2, negate_literal l /= lit])