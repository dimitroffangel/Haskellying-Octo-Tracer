module Vector where

import Data.Word

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

data Rgb = Rgb {    red:: Word8, 
                    green :: Word8, 
                    blue  :: Word8 
                } deriving (Show,Read, Eq)
