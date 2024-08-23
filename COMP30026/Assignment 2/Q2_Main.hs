import RegExp -- do not remove
import Hidden -- do not remove

-- TODO: put your definitions here
rA,rB,rC :: RegExp
rA = parseRE "b*(a | (abb*)*(eps | a))"
rB = parseRE "b*(ab*a)*b*"
rC = parseRE "b*(abb*a)*b*"