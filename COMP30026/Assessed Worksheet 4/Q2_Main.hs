import Data.List ((\\)) -- add more imports if required
import DFA
import VisDFA
import Hidden -- required for testing purposes


-- TODO: Complete definition for complementDFA
complementDFA :: DFA -> DFA
complementDFA dfa
  = dfa