module HitRecord where

import Ray
import Vector

data HitRecord = HitRecord {
        point :: Vector,
        normalVector :: Vector,
        t :: Double,
        frontFace :: Bool
    }

emptyHitRecord = HitRecord (Vector 0 0 0) (Vector 0 0 0) 0 False

setFaceNormal (HitRecord point _ t frontFace) (Ray origin direction) outwardNormal = 
    let newFrontFace = dotProduct direction outwardNormal < 0 
        in if newFrontFace 
                then HitRecord point outwardNormal t newFrontFace
                else HitRecord point (-outwardNormal) t newFrontFace


data LambertianMaterial = LambertianMaterial {
    colour :: Vector
} 

getScatteredRay lambertianMaterial hit incomingRay@(Ray origin direction) unitSphereVector
    | isVectorNearZero unitSphereVector = getScatteredRay lambertianMaterial hit incomingRay $ Vector 0 0 0 
    | otherwise = Ray (point hit) $ normalVector hit + unitSphereVector

gelColourAfterIncomingArrayOnLambertianMaterial  lambertianMaterial _ = colour lambertianMaterial  