module Sphere where


import Ray
import HitRecord
import Vector

data Sphere = Sphere {
    center :: Vector,
    radius :: Double
}

hit (Sphere sphereCenter sphereRadius) ray@(Ray rayOrigin rayDirection) 
    tMin tMax hitRecord@(HitRecord hitRecordPoint hitRecordNormal hitRecordT hitRecordFrontFace) = 
    let 
        vectorFromTheCenter = rayOrigin - sphereCenter
        a =  getSquaredVector rayDirection
        halfB = dotProduct vectorFromTheCenter rayDirection
        c = getSquaredVector vectorFromTheCenter - sphereRadius^2
        discriminant = (halfB*halfB) - (a*c)
        in if discriminant < 0 
            then Left hitRecordFrontFace
            else let 
                    negativeB = -halfB
                    sqrtDiscriminant = sqrt discriminant
                    root = (negativeB - sqrtDiscriminant) / a
                    in if root < tMin || root > tMax 
                        then let root = (negativeB + sqrtDiscriminant) / a
                                in if root < tMin || root > tMax 
                                    then Left hitRecordFrontFace
                                    else let getRecordAfterOutwardNormal = setFaceNormal hitRecord ray $ 
                                                scalarDivision (hitRecordPoint - sphereCenter) sphereRadius
                                            in Right $ HitRecord (getPointLocation ray hitRecordT) (normalVector getRecordAfterOutwardNormal) root $
                                                    frontFace getRecordAfterOutwardNormal
                                                 
                        else let getRecordAfterOutwardNormal = setFaceNormal hitRecord ray $ scalarDivision (hitRecordPoint - sphereCenter) sphereRadius
                                in Right $ HitRecord (getPointLocation ray hitRecordT) (normalVector getRecordAfterOutwardNormal) root $
                                                    frontFace getRecordAfterOutwardNormal
