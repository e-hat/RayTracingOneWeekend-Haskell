-- Written by Eddie Hatfield, February 2021.
-- Based on Ray Tracing in One Weekend tutorial by Peter Shirley.

import System.IO
import Data.List

imageWidth = 256
imageHeight = 256

writePixel w h x y =
    let r = fromIntegral x / (fromIntegral w - 1.0)
        g = fromIntegral y / (fromIntegral h - 1.0)
        b = 0.25
        ir = floor (255.999 * r)
        ig = floor (255.999 * g)
        ib = floor (255.999 * b)
        irgb = map (show) [ir,ig,ib]
    in concat (intersperse " " irgb) ++ "\n"

genImg w h =
    "P3\n" ++
    (show w) ++ " " ++ 
    (show h) ++ 
    "\n255\n" ++
     (foldr (++) "" [writePixel w h x y | y <- [h - 1, h - 2..0], x <- [0..w - 1]])

main = do
    writeFile "output/output.ppm" (genImg imageWidth imageHeight)
