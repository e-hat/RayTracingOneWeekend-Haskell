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
  | HittableList [Hittable]
  deriving Show

data HitRecord = HitRecord { getP    :: Vec3 
                           , getNorm :: Vec3 
                           , getT    :: Double
                           }

hit :: Ray -> (Double, Double) -> Hittable -> Maybe HitRecord
hit ray@(Ray origin dir) (tMin, tMax) (Sphere c r) =
    let 
        oc = origin `subV` c
        a  = len2 dir
        halfB = dot oc dir
        cv = len2 oc - r * r
        discriminant = halfB * halfB - a * cv
        sqrtd = sqrt discriminant
        root1 = (-halfB - sqrtd) / a
        root2 = (-halfB + sqrtd) / a
        checkRootRange r = if r > tMin && r < tMax then Just r else Nothing
        setNormalDir outNorm = outNorm `scl` signum (dir `dot` outNorm) `scl` (-1)
    in do
        guard (discriminant >= 0)
        root <- checkRootRange root1 <|> checkRootRange root2
        let p = ray `at` root
        return $ HitRecord p (setNormalDir $ p `subV` c `shrink` r) root

hit r tRange (HittableList objs) = 
    getHitRecord <$> mconcat (map ((HitListItem <$>) . hit r tRange) objs)

newtype HitListItem = HitListItem { getHitRecord :: HitRecord }

instance Semigroup HitListItem where
    h1 <> h2 = if getT (getHitRecord h1) < getT (getHitRecord h2) then h1 else h2
