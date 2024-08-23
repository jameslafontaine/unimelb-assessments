module Svg (pathToSvg) where

import Data.List

pathToSvg :: [(Double, Double)] -> IO ()
pathToSvg ps = writeFile "___media.svg" (pre ++ pic ++ post) 
    where 
        pre = svgPreamble (1 + newWidth) (1 + newHeight) 
        pic = (pathToSvgPolyline . movePath . scalePath) psTrunc
        post = "</svg>"
        (xmin, ymin, xmax, ymax) = (getBounds psTrunc)        
        width = xmax - xmin
        height = ymax - ymin
        scaleFactor = 500 / (maximum [width, height])
        newWidth = scaleFactor * width
        newHeight = scaleFactor * height
        scalePath = map (both ((*) scaleFactor))
        movePath = map (fprod ((+) (0.5 - xmin * scaleFactor)) ((+) (0.5 - ymin * scaleFactor)))
        psTrunc = take 33000 ps
        
        

pathToSvgPolyline :: [(Double, Double)] -> String
pathToSvgPolyline ps = pre ++ intercalate " " (map svgshow ps) ++ post 
    where 
        pre = "<polyline points=\""
        post = "\" style=\"fill:none;stroke:black;stroke-width:0.5\"/>\n"
        svgshow = tail . init . show

svgPreamble :: Double -> Double -> String
svgPreamble width height = "<svg version=\"1.1\" \
    \xmlns=\"http://www.w3.org/2000/svg\" \
    \width=\"" ++ (show width) ++ "\" height=\"" ++ (show height) ++ "\" \
    \style=\"background-color:white\">\n"

fprod :: (a -> b) -> (c -> d) -> ((a, c) -> (b, d))
fprod f g (x, y) = (f x, g y)

both :: (a -> b) -> ((a, a) -> (b, b))
both f (x, y) = fprod f f (x, y) 

getBounds :: [(Double, Double)] -> (Double, Double, Double, Double)
getBounds ps = (xmin, ymin, xmax, ymax)
    where 
        (xmin, ymin) = (both minimum) (unzip ps)
        (xmax, ymax) = (both maximum) (unzip ps)
