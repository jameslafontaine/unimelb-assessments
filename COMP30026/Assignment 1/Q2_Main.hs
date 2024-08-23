-- Please don't remove or alter these imports, 
-- as they are required for marking.
import Q2_types
import Hidden

-- Task 2A  
task2A :: Exp
task2A = helper2A (IMPL (IMPL (NOT (VAR 'P')) (IMPL (AND (NOT (VAR 'Q')) (VAR 'Q')) FALSE)) ( IMPL (IMPL (VAR 'P') (AND (NOT (IMPL (VAR 'R') (VAR 'Q'))) (NOT (VAR 'P')))) (NOT (IMPL (VAR 'P') (NOT (IMPL (VAR 'R') (VAR 'Q')))))))

helper2A :: Exp -> Exp
helper2A (VAR x)
  = VAR x
  
helper2A FALSE
  = FALSE

helper2A (AND f g) 
  = AND (helper2A f) (helper2A g)

helper2A (IMPL f g)
  = NOT (AND (helper2A f) (helper2A (NOT g)))

helper2A (NOT f)
  = NOT (helper2A f) 
    
-- Task 2B
task2B :: Exp
task2B = NOT (AND (NOT (VAR 'P')) (NOT (AND (NOT (VAR 'P')) ( AND ( AND (NOT (VAR 'Q')) (VAR 'Q') ) (NOT FALSE) ) ) ) ) 

-- Task 2C
task2C :: Exp -> Exp

-- stop at variables 
task2C (VAR x)
  = VAR x
  
-- convert constants to statements using only AND, NOT, and VAR  
  
task2C TRUE 
  = task2C (OR (NOT (VAR 'A')) (VAR 'A'))
  
task2C FALSE
  = AND (VAR 'A') (NOT (VAR 'A'))
  
-- remove double negations
task2C (NOT (NOT f)) 
  = task2C f
  
-- transform all connectives to AND and NOT

task2C (NOT f)
    | f == FALSE = task2C TRUE
    | f == TRUE = task2C FALSE
    | otherwise = NOT (task2C f)

task2C (AND f g) 
  = AND (task2C f) (task2C g)

task2C (OR f g) 
  = NOT (AND (task2C (NOT f)) (task2C (NOT g)))

task2C (XOR f g)
  = AND (task2C (OR f g)) (task2C (OR (NOT f) (NOT g)))
  
task2C (BIIM f g)
  = AND (task2C (IMPL f g)) (task2C (IMPL g f))
  
task2C (IMPL f g)
  = NOT (AND (task2C f) (task2C (NOT g)))
  
-- XOR test 1 (XOR (XOR (OR (NOT (VAR 'P')) (VAR 'Q')) (OR (VAR 'P') (VAR 'Q'))) (XOR (OR (NOT (VAR 'P')) (NOT (VAR 'Q'))) (OR (VAR 'P') (NOT (VAR 'Q')))))
-- XOR test 2 (XOR (OR (NOT (VAR 'P')) (VAR 'Q')) (OR (VAR 'P') (VAR 'Q'))) 
                                       
 