module HitRecord where

import Ray
import Vector


clampFuzziness :: Double -> Double
clampFuzziness a 
    | a < 1 = a
    | otherwise = 1

data Material = LambertianMaterial {
        colour :: Vector
    }
    | Metal {
        colour :: Vector,
        fuzziness :: Double
    }
    | Void


data HitRecord = HitRecord {
        point :: Vector,
        normalVector :: Vector,
        hitRecordMaterial :: Material,
        t :: Double,
        frontFace :: Bool
    }

emptyHitRecord = HitRecord (Vector 0 0 0) (Vector 0 0 0) Void 0 False

setFaceNormal (HitRecord point _ material t frontFace) (Ray origin direction) outwardNormal = 
    let newFrontFace = dotProduct direction outwardNormal < 0 
        in if newFrontFace 
                then HitRecord point outwardNormal  material t newFrontFace
                else HitRecord point (-outwardNormal) material t newFrontFace



getScatteredRay lambMaterial@(LambertianMaterial _) hit incomingRay@(Ray origin direction) unitSphereVector
    | isVectorNearZero unitSphereVector = getScatteredRay lambMaterial hit incomingRay $ Vector 0 0 0 
    | otherwise = Right $ Ray (point hit) $ normalVector hit + unitSphereVector

getScatteredRay (Metal colour fuzziness) hit ray@(Ray origin direction) unitSphereVector = 
    let result@(Ray resultOrigin resultDirection)= Ray (point hit) $ (reflect (getUnitVector direction) (normalVector hit)) + 
                scalarMultiplication unitSphereVector fuzziness 
        in if (dotProduct resultDirection (normalVector hit)) > 0
            then Right result
            else Left result

gelColourAfterRay (LambertianMaterial colour) _ = colour  
gelColourAfterRay (Metal colour _)  _ = colour  