import DPDA
import VisDPDA
import RunDPDA  -- For testing; do not remove
import Hidden   -- For testing; do not remove

dpdaL, dpdaR :: DPDA

dpdaL = ([1,2,3], "abc", "ab$", delta, 1, [1])
    where
      delta = [ ((1,'a',eps), (2,'$'))
              , ((1,'b',eps), (3,'$'))
              , ((1,'c',eps), (1,eps))
              , ((2,'a',eps), (2,'a'))
              , ((2,'b','a'), (2,eps))
              , ((2,'c',eps), (2,eps))
              , ((2,'b','$'), (1,eps))
              , ((3,'b',eps), (3,'b'))
              , ((3,'a','b'), (3,eps))
              , ((3,'c',eps), (3,eps))
              , ((3,'a','$'), (1,eps))
              ]

dpdaR = ([1,2,3,4,5,6], "abc", "abc#$%", delta, 1, [6])
    where
      delta = [ ((1,'a',eps), (2,'#'))
              , ((1,'b',eps), (5,'#'))
              , ((2,'a',eps), (2,'a'))
              , ((2,'b',eps), (3,eps))
              , ((3,'b','a'), (4,eps))
              , ((4,'b',eps), (3,eps))
              , ((3,'b','#'), (3,'$'))
              , ((3,'b','$'), (3,'%'))
              , ((3,'b','%'), (5,'%'))
              , ((5,'b',eps), (5,'b'))
              , ((5,'c','b'), (6,eps))
              , ((6,'c','a'), (6,eps))
              , ((6,'c','b'), (6,eps))
              , ((6,'c','%'), (3,eps))
              ]