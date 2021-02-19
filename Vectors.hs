module Vectors
( Vec3 (..)
, point3 
, color
, addV
, subV
, scl
, shrink
, len
, len2
, dot
, cross
, dir
) where

data Vec3 = Vec3 { getX :: Double 
                 , getY :: Double 
                 , getZ :: Double 
                 } deriving (Eq,Show)
point3 = Vec3
color = Vec3

addV :: Vec3 -> Vec3 -> Vec3
addV (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

subV :: Vec3 -> Vec3 -> Vec3
subV (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

scl :: Vec3 -> Double -> Vec3
scl (Vec3 x y z) t = Vec3 (t * x) (t * y) (t * z)

shrink :: Vec3 -> Double -> Vec3
shrink v 0 = error "Attempting divide by zero in 'shrink'"
shrink v t = scl v (1.0 / t)

len2 :: Vec3 -> Double
len2 v = dot v v

len :: Vec3 -> Double
len v = sqrt $ len2 v

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

dir :: Vec3 -> Vec3
dir (Vec3 0 0 0) = Vec3 0 0 0
dir v = shrink v (len v)
