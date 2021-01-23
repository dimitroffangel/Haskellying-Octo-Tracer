module MovingSphere where

import Ray
import HitRecord
import Vector
import HittableTypes
import AxisAlignedBoundingBox


moveSphere (MovingSphere centerInitially centerAfterTime _ _ movingFromTime movingUntilTime ) time =
    centerInitially + (scalarMultiplication (centerInitially - centerAfterTime) $ (time - movingFromTime) / (movingUntilTime - movingFromTime))


-- sphere equation for point P:(x,y,z) and sphere with center C:(Cx, Cy, Cz) and radius R
-- (x - Cx)^2 + (y - Cy)^2 + (z - Cz)^2 = R^2
-- the vector from the center of the sphere is (P -C) . (P - C)
-- hence (P - C)(P - C) = (x - Cx)^2 + (y - Cy)^2 + (z - Cz)^2 = R^2
-- and with the ray data for P:
-- (P(t) - C)(P(t) - C) = R^2
-- (origin + t*direction - C)(origin + t*direction - C) = R^2
-- t^2*direction*direction + 2t*direction * (origin -C) + (origin - C)(origin - C) - R^2 = 0
-- if has two roots - two collisions with outer sphere and so forth
hitMovingSphere movingSphere@(MovingSphere sphereCenterInitially sphereCenterAfterTime sphereRadius sphereMaterial fromTime untilTime) ray@(Ray rayOrigin rayDirection rayTime) 
    tMin tMax hitRecord@(HitRecord hitRecordPoint hitRecordNormal hitRecordMaterial hitRecordT hitRecordFrontFace) = 
    let 
        movedCenter = (moveSphere movingSphere rayTime)
        vectorFromTheCenter = rayOrigin - movedCenter
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
                                                scalarDivision (newPoint - movedCenter) sphereRadius
                        else let 
                                newT = firstRoot
                                newPoint = (getPointLocation ray newT)
                                in Right $ setFaceNormal (HitRecord newPoint hitRecordNormal sphereMaterial newT hitRecordFrontFace) ray $ 
                                    scalarDivision (newPoint - sphereCenterInitially) sphereRadius




movingSphereBoundBox movingSphere@(MovingSphere sphereCenterInitially sphereCenterAfterTime sphereRadius sphereMaterial fromTime untilTime) fromInterval toInterval = 
    let 
        boxBeforeMovement = AABB 
                (moveSphere movingSphere fromInterval + (Vector sphereRadius sphereRadius sphereRadius))
                (moveSphere movingSphere fromInterval + (Vector sphereRadius sphereRadius sphereRadius))
        boxAfterMovement = AABB 
                (moveSphere movingSphere toInterval + (Vector sphereRadius sphereRadius sphereRadius))
                (moveSphere movingSphere toInterval + (Vector sphereRadius sphereRadius sphereRadius))
        in makeSurroundingBox boxBeforeMovement boxAfterMovement