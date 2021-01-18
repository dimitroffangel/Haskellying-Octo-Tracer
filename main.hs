module Task2 where

import Vector
import IO
import Ray


-- image info
aspectRatio = 16.0 / 9.0
imageWidth = 400
imageHeight = floor $ realToFrac imageWidth / aspectRatio


-- use of Right hand coordiante system
-- camera stuff
viewportHeight = 2
viewportWidth = aspectRatio * realToFrac viewportHeight
-- distance between projection plane and the projection point
focalLength = 1.0

-- eye coordinate
originLocation = Vector 0 0 0
horizontal = Vector (realToFrac viewportWidth) 0 0
vertical = Vector 0 viewportHeight 0
lowerLeftCorner = (originLocation - scalarDivision horizontal 2) - (scalarDivision vertical 2 - Vector 0 0 focalLength) 

testingPicture currentWidth currentHeight result
    | currentWidth == imageWidth && currentHeight == 0 = Image imageWidth imageHeight $ splitListOnLists imageWidth $ reverse result
    | currentWidth == imageWidth = testingPicture 0 (currentHeight - 1) result
    | otherwise = 
        testingPicture (currentWidth + 1) currentHeight 
            $ let u = realToFrac currentWidth / realToFrac (imageWidth - 1)
                  v = realToFrac currentHeight / realToFrac (imageHeight - 1)
                  ray = Ray originLocation (lowerLeftCorner + (scalarMultiplication horizontal u) + ((scalarMultiplication vertical v) - originLocation)) 
                  pixelColour = rayColour ray
                  in pixelColour : result

runTest = saveImage (testingPicture 0 (imageHeight -1) []) "./foo.ppm"