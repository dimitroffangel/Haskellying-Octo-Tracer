module Camera where

import Vector

import Ray

-- image info
aspectRatio = 16.0 / 9.0
imageWidth = 400
imageHeight = floor $ realToFrac imageWidth / aspectRatio

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

getRay u v = Ray originLocation (lowerLeftCorner + scalarMultiplication horizontal u + (scalarMultiplication vertical v - originLocation)) 

data Camera = Camera {
    cameraOrigin :: Vector,
    cameraLowerLeftCorner :: Vector, 
    cameraHorizontal:: Vector,
    cameraVertical :: Vector
} deriving (Show, Read, Eq)