-- add more imports if required
import DFA
import NFA
import VisDFA
import Hidden -- required for testing purposes

-- TODO: Complete definition for reverseDFA
reverseDFA :: DFA -> NFA
reverseDFA (qs, xs, ts, q0, as)
  = (qs, xs, ts, [q0], as)
