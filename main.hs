module Task2 where

import Vector
import IO
import Ray
import HitRecord
import Sphere
import Hittable
import GeneratingRandomStuff
import Camera
import UtilityFunctions

-- constants
infinity = 1.79769e+308

world = [
        Sphere (Vector 0 0 (-1)) 0.5,
        Sphere (Vector 0 (-100.5) (-1)) 100
    ]

mainCamera = Camera originLocation lowerLeftCorner horizontal vertical

-- linearly blends white and blue depending on y cooridnate of the unit vector of the ray direction
rayColour ray@(Ray origin direction) worldObjects =
    let hitResult = hitList worldObjects ray 0 infinity emptyHitRecord
        in case hitResult of 
            (Right hit) -> scalarMultiplication ((normalVector hit) + Vector 1 1 1) 0.5
            (Left _) -> 
                let (Vector _ y _) = getUnitVector direction
                    t' = 0.5 * (y + 1)
                in (scalarMultiplication (Vector 1.0 1.0 1.0) (1 - t')) + (scalarMultiplication (Vector 0.5 0.7 1) t')


shadePixel 0 _ _ resultedColour = 
    do 
        foo <- generateNumberInInterval 0 0
        return resultedColour
shadePixel index width height resultedColour =
    do
        randomGeneratedValueU <- generateNumberInInterval 0 1
        randomGeneratedValueV <- generateNumberInInterval 0 1
        let 
            u =  ((realToFrac width) + randomGeneratedValueU) / realToFrac (imageWidth - 1)
            v = ((realToFrac height) + randomGeneratedValueV) / realToFrac (imageHeight - 1)
            ray = getRay u v mainCamera 
            in shadePixel (index - 1) width height (resultedColour + rayColour ray world)

testingPicture currentWidth currentHeight result
    | currentWidth == imageWidth && currentHeight == 0 = 
        do 
            foo <- generateNumberInInterval 0 0
            return $ Image imageWidth imageHeight $ splitListOnLists imageWidth $ reverse result
    | currentWidth == imageWidth = testingPicture 0 (currentHeight - 1) result
    | otherwise =  do
                resultedPixel <- shadePixel samplePerPixel currentWidth currentHeight (Vector 0 0 0)
                testingPicture (currentWidth + 1) currentHeight (resultedPixel : result)
                

runTest = saveImage (testingPicture 0 (imageHeight -1) []) "./foo.ppm"