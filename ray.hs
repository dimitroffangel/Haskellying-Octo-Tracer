module Ray where

import Data.Word

import Vector

--  P(t)= origin+t*direction
data Ray = Ray {
    origin :: Vector,
    direction :: Vector 
} deriving (Show, Read, Eq)

getPointLocation (Ray origin direction) t = origin + t * direction  


-- linearly blends white and blue depending on y cooridnate of the unit vector of the ray direction
rayColour (Ray origin direction) = 
    let unitDirection = getUnitVector direction 
        t = 0.5 * (y unitDirection + 1)
        -- blend the value (1-t)*StartingValue + t*endValue
    in scalarMultiplication (Vector 1.0 1.0 1.0) (1 - t) + scalarMultiplication (Vector 0.5 0.7 1) t