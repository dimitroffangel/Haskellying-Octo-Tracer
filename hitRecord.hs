module HitRecord where

import Ray
import Vector
import GeneratingRandomStuff


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
    | Dielectric{
        indexOfRefraction :: Double
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



getScatteredRay lambMaterial@(LambertianMaterial _) hit incomingRay@(Ray origin direction) unitSphereVector randomNumber
    | isVectorNearZero unitSphereVector = getScatteredRay lambMaterial hit incomingRay (Vector 0 0 0) randomNumber
    | otherwise = Right $ Ray (point hit) $ normalVector hit + unitSphereVector

getScatteredRay (Metal colour fuzziness) hit incomingRay@(Ray origin direction) unitSphereVector _ = 
    let result@(Ray resultOrigin resultDirection)= Ray (point hit) $ (reflect (getUnitVector direction) (normalVector hit)) + 
                scalarMultiplication unitSphereVector fuzziness 
        in if (dotProduct resultDirection (normalVector hit)) > 0
            then Right result
            else Left result

getScatteredRay (Dielectric indexOfRefraction) hit incomingRay@(Ray origin direction) unitSphereVector randomNumber = 
    if frontFace hit 
        then refractReflectRay $ 1 / indexOfRefraction
        else refractReflectRay indexOfRefraction 
        where
            unitDirection = getUnitVector direction
            cosTheta = min (dotProduct (-unitDirection) $ normalVector hit) 1
            sinTheta = sqrt (1 - cosTheta * cosTheta) 
            refractReflectRay refractionRatio 
            -- cannot refract because the sin of theta' gets above 1, so reflect 
                | (refractionRatio * sinTheta > 1) || (reflectance cosTheta refractionRatio) > randomNumber = 
                    Right $ Ray (point hit) $ reflect unitDirection $ normalVector hit
                | otherwise = Right $ Ray (point hit) $ refract unitDirection (normalVector hit) refractionRatio

getColourAfterRay (LambertianMaterial colour) = colour  
getColourAfterRay (Metal colour _) = colour  
getColourAfterRay (Dielectric _) = Vector 1 1 1

reflectance cosine indexOfReflactance =
    let r0 = (1 - indexOfReflactance) / (1 + indexOfReflactance)
        raisedro = r0 * r0
        in raisedro + (1-raisedro)* ((1-cosine)^5)