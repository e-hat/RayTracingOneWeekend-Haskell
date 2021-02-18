module Hittable
( Hittable (..) 
, HitRecord (..)
, hit
) where

import Vectors
import Ray
import Data.Monoid
import Control.Applicative
import Control.Monad

data Hittable = 
    Sphere Vec3 Double

data HitRecord =
    HitRecord Vec3 Vec3 Double

hit :: Hittable -> Ray -> (Double, Double) -> Maybe HitRecord
hit (Sphere c r) ray@(Ray rOrigin rDir) (tMin, tMax) =
    let 
        oc = rOrigin `subV` c
        a  = len2 rDir
        halfB = dot oc rDir
        cv = len2 oc - r * r
        discriminant = halfB * halfB - a * cv
        sqrtd = sqrt discriminant
        root1 = (-halfB - sqrtd) / a
        root2 = (-halfB + sqrtd) / a
        checkRootRange r = if r > tMin && r < tMax then Just r else Nothing
    in do
        guard (discriminant < 0)
        root <- checkRootRange root1 <|> checkRootRange root2
        let p = ray `at` root
        return $ HitRecord p (p `subV` c `shrink` r) root
