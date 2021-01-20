module Vector where

import Data.Word

import GeneratingRandomStuff

data Vector = Vector {
    x :: Double,
    y :: Double,
    z :: Double
} deriving (Show, Read, Eq)

instance Num Vector where
    (Vector x1 y1 z1) + (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)
    (Vector x1 y1 z1) - (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)
    (Vector x1 y1 z1) * (Vector x2 y2 z2) = Vector (x1 * x2) (y1 * y2) (z1 * z2)
    negate (Vector x1 y1 z1) = Vector (-x1) (-y1) (-z1)
    abs (Vector x y z) = Vector (abs x) (abs y) (abs z)
    signum (Vector x y z) = Vector (signum x) (signum y) (signum z)
    fromInteger i = Vector (fromInteger i) (fromInteger i) (fromInteger i)


scalarMultiplication (Vector x y z) scalar = Vector (x * scalar) (y * scalar)  (z * scalar)

scalarDivision (Vector x y z) scalar = Vector (x / scalar) (y / scalar)  (z / scalar)

getVectorLength a = sqrt $ getSquaredVector a

getSquaredVector (Vector x y z) = x^2 + y^2 + z^2 

getUnitVector vector = scalarDivision vector $ getVectorLength vector

dotProduct (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1*y2 + z1*z2

crossProduct (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (y1 * z2 - z1*y2) (z1*x2 - z2*x1) (x1*y2 - y1*x2) 

getRandomVectorInInterval min max =
    do 
        x <- generateNumberInInterval min max
        y <- generateNumberInInterval min max
        z <- generateNumberInInterval min max
        return $ Vector x y z

getRandomVectorInUnitSphere =
    do 
        x <- generateNumberInInterval (-1) 1
        y <- generateNumberInInterval (-1) 1
        if x^2 + y^2 > 1
            then getRandomVectorInUnitSphere
            else return $ Vector x y (1 - (x^2 + y^2)) 
        

getUnitVectorInUnitSphere = getRandomVectorInUnitSphere

epsilon = 1e-8

isVectorNearZero (Vector x y z) = x < epsilon && y < epsilon && z < epsilon 

reflect :: Vector -> Vector -> Vector
reflect incomingRay normalVector = incomingRay - (scalarMultiplication normalVector $ 2 * dotProduct incomingRay normalVector)

refract uv  normal etaOverEtaPrim =
    let cosTheta = min (dotProduct (-uv) normal) 1
        refractedRayPerpendicular = scalarMultiplication (uv + scalarMultiplication normal cosTheta) etaOverEtaPrim
        refractedRayParallel = scalarMultiplication normal $ -sqrt(abs $ 1 - getSquaredVector refractedRayPerpendicular)  
        in refractedRayPerpendicular + refractedRayParallel 

type Rgb = Vector
