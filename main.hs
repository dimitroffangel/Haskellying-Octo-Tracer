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
demoRadius = cos (pi / 4)
world = [
        Sphere (Vector (-demoRadius) 0 (-1)) demoRadius (LambertianMaterial (Vector 0 0 1)),
        Sphere (Vector demoRadius 0 (-1)) demoRadius (LambertianMaterial (Vector 1 0 0))
    ]

mainCamera = constructCamera 90 aspectRatio

maxDepth = 50

transformEitherToIO :: Either a a -> IO a
transformEitherToIO (Right a) = return a
transformEitherToIO (Left a) = return a

wrapInIO :: a -> IO a
wrapInIO = return

-- linearly blends white and blue depending on y cooridnate of the unit vector of the ray direction
rayColour ray@(Ray origin direction) worldObjects 0 = 
    do 
        foo <- generateNumberInInterval 0 0
        return $ Vector 0 0 0
rayColour ray@(Ray origin direction) worldObjects depth =
    let hitResult = hitList worldObjects ray 0.001 infinity emptyHitRecord
        in case hitResult of 
            (Right hit) -> 
                do 
                    -- using Lambertian reflection (hence all light is reflected equally in all directions), 
                    -- where there is a higher probability for rays that close to the normal with a more uniform distrubution cosfi,
                    -- where fi is the angle between the normal of the sphere and the point
                    -- get a random point in the sphere and normalize it to be on the sphere
                    unitSphereVector <- getUnitVectorInUnitSphere
                    randomNumber <- (generateNumberInInterval 0 1)
                    getNewRay <- 
                        let scatteredRay = getScatteredRay (hitRecordMaterial hit) hit ray unitSphereVector randomNumber
                            in case scatteredRay of
                                (Left _) -> return $ Vector 0 0 0
                                (Right finalResult) -> (rayColour finalResult worldObjects $ depth - 1)
                    return $ getNewRay * (getColourAfterRay $ hitRecordMaterial hit)  

            (Left _) -> 
                let (Vector _ y _) = getUnitVector direction
                    t = 0.5 * (y + 1)
                in 
                    do 
                        foo <- generateNumberInInterval 0 0
                        return $ (scalarMultiplication (Vector 1.0 1.0 1.0) (1 - t)) + (scalarMultiplication (Vector 0.5 0.7 1) t)


shadePixel 0 _ _ resultedColour = 
    do 
        foo <- generateNumberInInterval 0 0
        return resultedColour
shadePixel index width height resultedColour =
    do
        randomGeneratedValueU <- generateNumberInInterval 0 1
        randomGeneratedValueV <- generateNumberInInterval 0 1
        unwrappedColour <- 
            let 
                u =  ((realToFrac width) + randomGeneratedValueU) / realToFrac (imageWidth - 1)
                v = ((realToFrac height) + randomGeneratedValueV) / realToFrac (imageHeight - 1)
                ray = getRay u v mainCamera 
                in (rayColour ray world maxDepth)
        shadePixel (index - 1) width height (resultedColour + unwrappedColour) 

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