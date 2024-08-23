-- Please don't remove or alter these imports, 
-- as they are required for marking.
import Q1_types
import Hidden

-- Task 1A
haraldKnows :: Exp
haraldKnows = parse "((C <+> P) & ((C <=> (T & W)) & (P <=> (M => W))))"

-- Task 1B
possibilities :: [TruthAssignmentCPMTW]
possibilities = [ (False, True, False, False, False),
                  (False, True, False, False, True),
                  (False, True, False, True, False),
                  (False, True, True, False, True) ]

-- Task 1C
weatherOfMonday :: Weather
weatherOfMonday = Rain

weatherOfTuesday :: Weather
weatherOfTuesday = NoRain

weatherOfWednesday :: Weather
weatherOfWednesday = Rain