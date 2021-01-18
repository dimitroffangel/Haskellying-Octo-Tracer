module HitRecord where

import Ray
import Vector

data HitRecord = HitRecord {
        point :: Vector,
        normalVector :: Vector,
        t :: Double,
        frontFace :: Bool
    }

setFaceNormal (HitRecord point normal t frontFace) (Ray origin direction) outwardNormal = 
    let newFrontFace = dotProduct direction outwardNormal < 0 
        in if newFrontFace 
                then HitRecord point outwardNormal t newFrontFace
                else HitRecord point (-outwardNormal) t newFrontFace