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
-- world = [
--         Sphere (Vector (-demoRadius) 0 (-1)) demoRadius (LambertianMaterial (Vector 0 0 1)),
--         Sphere (Vector demoRadius 0 (-1)) demoRadius (LambertianMaterial (Vector 1 0 0))
--     ]
--
world = generateRandomScene (0) 4 (0) 4 []

-- world =   wrapInIO  [    
--             Sphere (Vector 0.0 (-100.5) (-1)) 100 (LambertianMaterial $ Vector 0.8 0.8 0),
--             Sphere (Vector 0.0 0.0 (-1.0)) 0.5 (LambertianMaterial $ Vector 0.1 0.2 0.5),
--             Sphere (Vector (-1) 0 (-1)) 0.5 (Dielectric 1.5),    
--             Sphere (Vector (-1) 0 (-1)) (-0.45) (Dielectric 1.5),
--             Sphere (Vector 1 0 (-1)) 0.5 (Metal (Vector 0.8 0.6 0.2) $ clampFuzziness 0) 
--             ]



lookFrom = Vector 13 2 3
lookAt = Vector 0 0 (0)
viewUp = Vector 0 1 0
distToFocus = 10
aperture = 0.1

-- mainCamera = constructCamera 90 aspectRatio (Vector (-2) 2 1) (Vector 0 0 (-1)) (Vector 0 1 0)
-- mainCamera = constructCamera 20 aspectRatio (Vector (-2) 2 1) (Vector 0 0 (-1)) (Vector 0 1 0)
mainCamera = constructCamera 20 aspectRatio aperture distToFocus lookFrom lookAt viewUp 0 1


maxDepth = 50

transformEitherToIO :: Either a a -> IO a
transformEitherToIO (Right a) = return a
transformEitherToIO (Left a) = return a

-- linearly blends white and blue depending on y cooridnate of the unit vector of the ray direction
rayColour ray@(Ray origin direction _) worldObjects 0 = wrapInIO $ Vector 0 0 0
rayColour ray@(Ray origin direction _) worldObjects depth =
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
                in wrapInIO $ (scalarMultiplication (Vector 1.0 1.0 1.0) (1 - t)) + (scalarMultiplication (Vector 0.5 0.7 1) t)


shadePixel 0 _ _ resultedColour _= wrapInIO resultedColour
shadePixel index width height resultedColour world =
    do
        randomGeneratedValueU <- generateNumberInInterval 0 1
        randomGeneratedValueV <- generateNumberInInterval 0 1
        unitDiskVector <- getRandomVectorInDiskCenter
        getRandomInitialStartOfRayCasting <- generateNumberInInterval (sendRayFromTime mainCamera) (sendRayUntilTime mainCamera) 
        unwrappedColour <- 
            let 
                u =  ((realToFrac width) + randomGeneratedValueU) / realToFrac (imageWidth - 1)
                v = ((realToFrac height) + randomGeneratedValueV) / realToFrac (imageHeight - 1)
                ray = getRay u v mainCamera unitDiskVector getRandomInitialStartOfRayCasting
                in (rayColour ray world maxDepth)
        shadePixel (index - 1) width height (resultedColour + unwrappedColour) world

testingPicture currentWidth currentHeight result world
    | currentWidth == imageWidth && currentHeight == 0 = wrapInIO $ Image imageWidth imageHeight $ splitListOnLists imageWidth $ reverse result
    | currentWidth == imageWidth = testingPicture 0 (currentHeight - 1) result world
    | otherwise =  do
                resultedPixel <- shadePixel samplePerPixel currentWidth currentHeight (Vector 0 0 0) world
                testingPicture (currentWidth + 1) currentHeight (resultedPixel : result) world
                

runTest = 
    do 
        getWorld <- world
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld) "./foo.ppm"