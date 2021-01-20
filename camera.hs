module Camera where

import Vector

import Ray


-- image info
aspectRatio = 16.0 / 9.0
imageWidth :: Int
imageWidth = 400
imageHeight :: Int
imageHeight = floor $ realToFrac imageWidth / aspectRatio
samplePerPixel = 10           

-- use of Right hand coordiante system
-- camera stuff
viewportHeight = 2
viewportWidth = ceiling $ aspectRatio * realToFrac viewportHeight
-- distance between projection plane and the projection point
focalLength = 1.0

-- eye coordinate
originLocation = Vector 0 0 0
horizontal = Vector (realToFrac viewportWidth) 0 0
vertical = Vector 0 2 0
lowerLeftCorner = originLocation - scalarDivision horizontal 2 - scalarDivision vertical 2 - Vector 0 0 focalLength

getRay u v (Camera cameraOrigin cameraLowerLeftCorner cameraHorizontal cameraVertical ) = 
    Ray cameraOrigin (cameraLowerLeftCorner + scalarMultiplication cameraHorizontal u + (scalarMultiplication cameraVertical v - cameraOrigin)) 

data Camera = Camera {
    cameraOrigin :: Vector,
    cameraLowerLeftCorner :: Vector, 
    cameraHorizontal:: Vector,
    cameraVertical :: Vector
} deriving (Show, Read, Eq)