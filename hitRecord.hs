module HitRecord where

import Ray
import Vector

data HitRecord = HitRecord {
        point :: Vector,
        normalVector :: Vector,
        t :: Double
    }