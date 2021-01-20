module Hittable where

import Vector
import Ray
import HitRecord
import Sphere


-- newtype HittableList = HittableList [Sphere]

clear hittableList = []

hitList hittableList ray@(Ray rayOrigin rayDirection) tMin tMax hitRecord =
    hitHelper hittableList tMax hitRecord False
        where 
            hitHelper [] _ tempRecord True = Right tempRecord 
            hitHelper [] _ tempRecord False = Left tempRecord
            hitHelper (currentHittable : rest) closestT tempHitRecord hasHit =
                let newHit = hit currentHittable ray tMin closestT tempHitRecord 
                in case newHit of 
                    (Left _) -> hitHelper rest closestT tempHitRecord hasHit
                    (Right resultHit) -> hitHelper rest (t resultHit) resultHit True
            
              