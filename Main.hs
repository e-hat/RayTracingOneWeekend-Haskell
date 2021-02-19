-- Written by Eddie Hatfield, February 2021.
-- Based on Ray Tracing in One Weekend tutorial by Peter Shirley.

import System.IO
import Data.List
import Data.Sequence hiding (intersperse)
import Vectors
import Ray
import Hittable

aspectRatio = 16.0 / 9.0
imageWidth = 400
imageHeight = floor $ fromIntegral imageWidth / aspectRatio

viewportHeight = 2.0
viewportWidth = aspectRatio * viewportHeight
focalLength = 1.0

origin = Vec3 0 0 0
horizontal = Vec3 viewportWidth 0 0
vertical = Vec3 0 viewportHeight 0
lowerLeftCorner = Main.origin `subV` shrink horizontal 2.0 `subV` shrink vertical 2.0 `subV` Vec3 0 0 focalLength

lerp :: Vec3 -> Vec3 -> Double -> Vec3
lerp u v t =
    u `scl` t `addV` (v `scl` (1.0 - t))

rayColor :: Ray -> Vec3
rayColor (Ray _ direction)  = 
    let unitDir = dir direction
        t = 0.5 * (getY unitDir + 1.0)
    in lerp (Vec3 0.5 0.7 1.0) (Vec3 1.0 1.0 1.0) t

colorString :: Vec3 -> String
colorString (Vec3 r g b) = 
    let toEightBitString x = show $ floor $ 255.999 * x 
        irgb = map toEightBitString [r,g,b]
    in unwords irgb ++ "\n"

pixel :: Int -> Int -> Int -> Int -> String
pixel w h x y =
    let u = fromIntegral x / (fromIntegral w - 1.0)
        v = fromIntegral y / (fromIntegral h - 1.0)
        r = Ray Main.origin (lowerLeftCorner `addV` scl horizontal u `addV` scl vertical v `subV` Main.origin)
    in colorString $ rayColor r

header :: Int -> Int -> String
header w h =
    "P3\n" ++
    show w ++ " " ++
    show h ++
    "\n255\n"

writeScanline :: Int -> Int -> Int -> IO String
writeScanline w h y = do
   putStr $ "Scanlines remaining: " ++ show y ++ "\n" 
   return $ concat [pixel w h x y | x <- [0..w - 1]]
   
genImg :: Int -> Int -> IO String
genImg w h = do
    contentAsLines <- sequenceA [writeScanline w h y | y <- [h - 1, h - 2..0]]
    return $ header w h ++ concat contentAsLines 

main :: IO ()
main = do
    img <- genImg imageWidth imageHeight
    writeFile "output/output.ppm" img 
    putStr "Done.\n"
