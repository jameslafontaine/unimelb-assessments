import ExtendedLSystem
import Svg

type Point = (Double, Double)

expandLSystem :: Int -> (LSystem a) -> [a]
-- copy over your solution to problem 1

extTurtleToPath :: [ExtTurtleAction] -> [Point]
-- adapt your solution to problem 2 to account for the new turtle actions

lSystemToTurtle :: Int -> (LSystem a) -> [ExtTurtleAction]
lSystemToTurtle r (start, rule, trans) = map trans (expandLSystem r (start, rule, trans))

lSystemToSvg :: Int -> (LSystem a) -> IO ()
lSystemToSvg r lsystem = (pathToSvg . extTurtleToPath) (lSystemToTurtle r lsystem)
