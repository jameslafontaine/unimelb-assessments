module LSystem (
    Angle, 
    Distance, 
    TurtleAction (..), 
    LSystem, 
    SierVar (..), 
    sierRule,
    sierTrans,
    sierSystem, 
    DragonVar (..),
    dragonRule,
    dragonTrans, 
    dragonSystem)
where

type Angle = Double
type Distance = Double 

data TurtleAction = Move Distance | Turn Angle

type LSystem a = ([a], a -> [a], a -> TurtleAction)

-- The Sierpinski triangle L-System 
data SierVar = SF | SG | SL | SR
    deriving (Show, Eq)

sierRule :: SierVar -> [SierVar]
sierRule SF = [SF, SL, SG, SR, SF, SR, SG, SL, SF]
sierRule SG = [SG, SG]
sierRule SL = [SL]
sierRule SR = [SR]

sierTrans :: SierVar -> TurtleAction
sierTrans SF = Move 1
sierTrans SG = Move 1
sierTrans SL = Turn ((2 * pi) / 3)
sierTrans SR = Turn (-(2 * pi) / 3)

sierSystem :: LSystem SierVar 
sierSystem = ([SF, SL, SG, SL, SG], sierRule, sierTrans)

-- Dragon curve L-System
data DragonVar = DF | DG | DL | DR
    deriving (Show, Eq)

dragonRule :: DragonVar -> [DragonVar]  
dragonRule DF = [DF, DR, DG]
dragonRule DG = [DF, DL, DG]
dragonRule DL = [DL]
dragonRule DR = [DR]

dragonTrans :: DragonVar -> TurtleAction 
dragonTrans DF = Move 1
dragonTrans DG = Move 1
dragonTrans DL = Turn ((pi / 2))
dragonTrans DR = Turn ((-pi) / 2)

dragonSystem :: LSystem DragonVar
dragonSystem = ([DF], dragonRule, dragonTrans)
