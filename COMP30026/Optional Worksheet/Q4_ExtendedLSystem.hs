module ExtendedLSystem (
    Angle,
    Distance,
    ExtTurtleAction(..), 
    LSystem, 
    BarnsleyVar,
    barnsleyRule,
    barnsleyTrans,
    barnsleySystem) where

type Angle = Double
type Distance = Double

data ExtTurtleAction = Turn Angle | Move Distance | NoAction | OpenTeleport | CloseTeleport

type LSystem a = ([a], a -> [a], a -> ExtTurtleAction)

-- Barnsley Fern L-System
data BarnsleyVar = Bx | Bf | Bl | Br | Bo | Bc

barnsleyRule :: BarnsleyVar -> [BarnsleyVar] 
barnsleyRule Bx = [Bf, Bl, Bo, Bo, Bx, Bc, Br, Bx, Bc, Br, Bf, Bo, Br, Bf, Bx, Bc, Bl, Bx]
barnsleyRule Bf = [Bf, Bf]
barnsleyRule Bl = [Bl]
barnsleyRule Br = [Br]
barnsleyRule Bo = [Bo] 
barnsleyRule Bc = [Bc]

barnsleyTrans :: BarnsleyVar -> ExtTurtleAction
barnsleyTrans Bx = NoAction
barnsleyTrans Bf = Move 1
barnsleyTrans Bl = Turn (pi * (5 / 32))
barnsleyTrans Br = Turn (-pi * (5 / 32))
barnsleyTrans Bo = OpenTeleport
barnsleyTrans Bc = CloseTeleport

barnsleySystem :: LSystem BarnsleyVar 
barnsleySystem = ([Bx], barnsleyRule, barnsleyTrans)