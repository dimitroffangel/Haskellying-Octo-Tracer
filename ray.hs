module Ray where

import Data.Word

import Vector

--  P(t)= origin+t*direction
data Ray = Ray {
    origin :: Vector,
    direction :: Vector 
} deriving (Show, Read, Eq)

getPointLocation (Ray origin direction) t = origin + scalarMultiplication direction t


-- linearly blends white and blue depending on y cooridnate of the unit vector of the ray direction
rayColour ray@(Ray origin direction) =
    let 
        test = hasHitSphere (Vector 0 0 $ -1) 0.5 ray
        in if test > 0
            then 
                let (Vector x y z)= getUnitVector (getPointLocation ray test + Vector 0 0 1)
                    in scalarMultiplication (Vector (x + 1) (y + 1) (z + 1)) 0.5
            else 
                -- blend the value (1-t)*StartingValue + t*endValue
                let unitDirection = getUnitVector direction
                    newT = 0.5 * (y unitDirection + 1)
                    in scalarMultiplication (Vector 1.0 1.0 1.0) (1 - newT) + scalarMultiplication (Vector 0.5 0.7 1) newT
                


-- sphere equation for point P:(x,y,z) and sphere with center C:(Cx, Cy, Cz) and radius R
-- (x - Cx)^2 + (y - Cy)^2 + (z - Cz)^2 = R^2
-- the vector from the center of the sphere is (P -C) . (P - C)
-- hence (P - C)(P - C) = (x - Cx)^2 + (y - Cy)^2 + (z - Cz)^2 = R^2
-- and with the ray data for P:
-- (P(t) - C)(P(t) - C) = R^2
-- (origin + t*direction - C)(origin + t*direction - C) = R^2
-- t^2*direction*direction + 2t*direction * (origin -C) + (origin - C)(origin - C) - R^2 = 0
-- if has two roots - two collisions with outer sphere and so forth

-- hasHitSphere sphereCenter sphereRadius (Ray rayOrigin rayDirection) = 
--     let vectorFromTheCenter = rayOrigin - sphereCenter
--         a = dotProduct rayDirection rayDirection
--         b = 2.0 * dotProduct vectorFromTheCenter rayDirection
--         c = dotProduct vectorFromTheCenter vectorFromTheCenter - sphereRadius^2
--         discriminant = (b*b) - (4*a*c)
--         negativeB = -b
--         sqrtDis = sqrt discriminant
--         quot = 2.0 * a
--         in if discriminant < 0 
--             then -1.0
--             else (negativeB - sqrtDis) / quot

hasHitSphere sphereCenter sphereRadius (Ray rayOrigin rayDirection) = 
    let vectorFromTheCenter = rayOrigin - sphereCenter
        a =  getSquaredVector rayDirection
        halfB = dotProduct vectorFromTheCenter rayDirection
        c = getSquaredVector vectorFromTheCenter - sphereRadius^2
        discriminant = (halfB*halfB) - (a*c)
        negativeB = -halfB
        sqrtDis = sqrt discriminant
        quot = a
        in if discriminant < 0 
            then -1.0
            else (negativeB - sqrtDis) / quot