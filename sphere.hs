module Sphere where


import Ray
import HitRecord
import Vector
import HittableTypes
import AxisAlignedBoundingBox


-- sphere equation for point P:(x,y,z) and sphere with center C:(Cx, Cy, Cz) and radius R
-- (x - Cx)^2 + (y - Cy)^2 + (z - Cz)^2 = R^2
-- the vector from the center of the sphere is (P -C) . (P - C)
-- hence (P - C)(P - C) = (x - Cx)^2 + (y - Cy)^2 + (z - Cz)^2 = R^2
-- and with the ray data for P:
-- (P(t) - C)(P(t) - C) = R^2
-- (origin + t*direction - C)(origin + t*direction - C) = R^2
-- t^2*direction*direction + 2t*direction * (origin -C) + (origin - C)(origin - C) - R^2 = 0
-- if has two roots - two collisions with outer sphere and so forth
hitSphere (Sphere sphereCenter sphereRadius sphereMaterial) ray@(Ray rayOrigin rayDirection _) 
    tMin tMax hitRecord@(HitRecord hitRecordPoint hitRecordNormal hitRecordMaterial hitRecordT hitRecordFrontFace) = 
    let 
        vectorFromTheCenter = rayOrigin - sphereCenter
        a =  getSquaredVector rayDirection
        halfB = dotProduct vectorFromTheCenter rayDirection
        c = getSquaredVector vectorFromTheCenter - (sphereRadius * sphereRadius)
        discriminant = (halfB*halfB) - (a*c)
        in if discriminant < 0 
            then Left hitRecord
            else let 
                    sqrtDiscriminant = sqrt discriminant
                    firstRoot = (-halfB - sqrtDiscriminant) / a
                    in if firstRoot < tMin || firstRoot > tMax 
                        then let secondRoot = (-halfB + sqrtDiscriminant) / a
                                in if secondRoot < tMin || secondRoot > tMax 
                                    then Left hitRecord
                                    else let 
                                            newT = secondRoot
                                            newPoint = (getPointLocation ray newT)
                                            in Right $ setFaceNormal (HitRecord newPoint hitRecordNormal sphereMaterial newT hitRecordFrontFace) ray $ 
                                                scalarDivision (newPoint - sphereCenter) sphereRadius
                        else let 
                                newT = firstRoot
                                newPoint = (getPointLocation ray newT)
                                in Right $ setFaceNormal (HitRecord newPoint hitRecordNormal sphereMaterial newT hitRecordFrontFace) ray $ 
                                    scalarDivision (newPoint - sphereCenter) sphereRadius




sphereBoundingBox (Sphere sphereCenter sphereRadius sphereMaterial) = 
    AABB 
        (sphereCenter - (Vector sphereRadius sphereRadius sphereRadius))
        (sphereCenter - (Vector sphereRadius sphereRadius sphereRadius))