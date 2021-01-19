module Hittable where

import Vector
import Ray
import HitRecord
import Sphere


data Hitabble = Sphere

newtype HittableList = HittableList [Sphere]

clear hittableList = []

hitList hittableList ray@(Ray rayOrigin rayDirection) tMin tMax hitRecord@(HitRecord hitRecordPoint hitRecordNormal hitRecordT hitRecordFrontFace) =
    hitHelper hittableList tMax EmptyHitRecord False
        where 
            hitHelper [] _ tempRecord True = Right tempRecord 
            hitHelper [] _ tempRecord False = Left tempRecord
            hitHelper (currentHittable : rest) closestT tempHitRecord hasHit =
                let newHit = hit currentHittable ray tMin closestT tempHitRecord 
                in case newHit of 
                    (Left resultHit) -> hitHelper rest closestT tempHitRecord hasHit
                    (Right resultHit) -> hitHelper rest (t resultHit) resultHit True
            
              