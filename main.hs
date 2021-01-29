-- Written by Eddie Hatfield, February 2021.
-- Based on Ray Tracing in One Weekend tutorial by Peter Shirley.

import System.IO
import Data.List
import Data.Sequence hiding (intersperse)
import Vectors
import Ray

aspectRatio = 16.0 / 9.0
imageWidth = 400
imageHeight = floor $ fromIntegral imageWidth / aspectRatio

viewportHeight = 2.0
viewportWidth = aspectRatio * viewportHeight
focalLength = 1.0

origin = Vec3 0 0 0
horizontal = Vec3 viewportWidth 0 0
vertical = Vec3 0 viewportHeight 0
lowerLeftCorner = Main.origin `subV` (shrink horizontal 2.0) `subV` (shrink vertical 2.0) `subV` (Vec3 0 0 focalLength)

lerp u v t =
    (scl u t) `addV` (scl v (1.0 - t))

rayColor r = 
    let unitDir = dir (direction r)
        t = 0.5 * (y unitDir + 1.0)
    in lerp (Vec3 0.5 0.7 1.0) (Vec3 1.0 1.0 1.0) t

colorString :: Vec3 -> String
colorString (Vec3 r g b) = 
    let toEightBitString x = show $ floor $ 255.999 * x 
        irgb = map (toEightBitString) [r,g,b]
    in (concat $ intersperse " " irgb) ++ "\n"

pixel w h x y =
    let r = fromIntegral x / (fromIntegral w - 1.0)
        g = fromIntegral y / (fromIntegral h - 1.0)
        b = 0.25
    in colorString $ Vec3 r g b

header w h =
    "P3\n" ++
    (show w) ++ " " ++
    (show h) ++
    "\n255\n"

writeScanline w h y = do
   putStr $ "Scanlines remaining: " ++ (show y) ++ "\n" 
   return $ foldr (++) "" [pixel w h x y | x <- [0..w - 1]] 
   
genImg w h = do
    let contentAsLinesIO = [writeScanline w h y | y <- [h - 1, h - 2..0]]
    contentAsLines <- (sequenceA contentAsLinesIO)
    return $ foldl (++) (header w h) contentAsLines 

main = do
    img <- genImg imageWidth imageHeight
    writeFile "output/output.ppm" img 
    putStr "Done.\n"
