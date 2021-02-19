module Ray 
( Ray (..)
, at
) where

import Vectors

data Ray = Ray { getOrigin     :: Vec3
               , getDirection  :: Vec3
               } deriving (Eq,Show)

at :: Ray -> Double -> Vec3
at (Ray o d) t = addV o (scl d t)
