import LSystem
import Svg

type Point = (Double, Double)

expandLSystem :: Int -> (LSystem a) -> [a]
-- copy over your solution to problem 1

turtleToPath :: [TurtleAction] -> [Point]
-- copy over your solution to problem 2

lSystemToTurtle :: Int -> (LSystem a) -> [TurtleAction]
lSystemToTurtle r (start, rule, trans) = map trans (expandLSystem r (start, rule, trans))

lSystemToSvg :: Int -> (LSystem a) -> IO ()
lSystemToSvg r lsystem = (pathToSvg . turtleToPath) (lSystemToTurtle r lsystem)
