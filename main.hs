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
import AxisAlignedBoundingBox
import HittableTypes
import Texture


-- constants
infinity = 1.79769e+308
demoRadius = cos (pi / 4)
-- world = [
--         Sphere (Vector (-demoRadius) 0 (-1)) demoRadius (LambertianMaterial (Vector 0 0 1)),
--         Sphere (Vector demoRadius 0 (-1)) demoRadius (LambertianMaterial (Vector 1 0 0))
--     ]
--

background0 = Vector 0 0 0
background1 = Vector 0.7 0.8 1
background2 = Vector 0.7 0.8 1
background3 = Vector 0.7 0.8 1

randomScene = generateRandomScene (0) 4 (0) 4 []
sceneWithTextureOfSinusAndCosinus = generateSecondScene 
sceneWithPerlinShader = generateThirdScene
simpleDiffuseTextureTest = sceneWithSimpleLight

depthOfFieldLookFrom = Vector (3) 3 2 
depthOfFieldLookAt = Vector 0 0 (-1)
depthOfFieldViewUp = Vector 0 1 0
depthOfFieldOfDistToFocus = getVectorLength (lookFrom - lookAt)
depthOfFieldAspectRatio = 16/ 9
depthOfFieldAperture = 2
depthOfFieldVerticalOfView = 20
depthOfFieldOfViewMainCamera = constructCamera depthOfFieldVerticalOfView depthOfFieldAspectRatio depthOfFieldAperture depthOfFieldOfDistToFocus 
                                                depthOfFieldLookFrom depthOfFieldLookAt depthOfFieldViewUp 0 1


-- lookFrom = Vector 26 3  6
lookFrom = Vector 13 2 3
lookAt = Vector 0 0 (0)
-- lookAt = Vector 0 2 0
viewUp = Vector 0 1 0
distToFocus = 10
aperture = 0.1

-- mainCamera = constructCamera 90 aspectRatio (Vector (-2) 2 1) (Vector 0 0 (-1)) (Vector 0 1 0)
-- mainCamera = constructCamera 20 aspectRatio (Vector (-2) 2 1) (Vector 0 0 (-1)) (Vector 0 1 0)
mainCamera = constructCamera 20 aspectRatio aperture distToFocus lookFrom lookAt viewUp 0 1
-- mainCamera = depthOfFieldOfViewMainCamera

maxDepth = 10

transformEitherToIO :: Either a a -> IO a
transformEitherToIO (Right a) = return a
transformEitherToIO (Left a) = return a

-- linearly blends white and blue depending on y cooridnate of the unit vector of the ray direction
rayColour :: (Eq a, Num a) => Ray -> Vector -> HittableObject -> a -> IO Vector
rayColour ray@(Ray origin direction _) backgroundColour worldObjects 0 = wrapInIO $ Vector 0 0 0
rayColour ray@(Ray origin direction _) backgroundColour worldObjects depth =
    let hitResult = hit worldObjects ray 0.001 infinity emptyHitRecord
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
                                (Left _) -> wrapInIO $ getEmittedMaterialColour (hitRecordMaterial hit) hit
                                (Right finalResult) -> (rayColour finalResult backgroundColour worldObjects $ depth - 1)
                    return $ (getEmittedMaterialColour (hitRecordMaterial hit) hit) + getNewRay * (getColourAfterRay (hitRecordMaterial hit) hit)  

            (Left _) -> wrapInIO backgroundColour


shadePixel 0 _ _ resultedColour _ _ = wrapInIO resultedColour
shadePixel index width height resultedColour world background =
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
                in (rayColour ray background world maxDepth)
        shadePixel (index - 1) width height (resultedColour + unwrappedColour) world background

testingPicture currentWidth currentHeight result world background 
    | currentWidth == imageWidth && currentHeight == 0 = wrapInIO $ Image imageWidth imageHeight $ splitListOnLists imageWidth $ reverse result
    | currentWidth == imageWidth = testingPicture 0 (currentHeight - 1) result world background
    | otherwise =  do
                resultedPixel <- shadePixel samplePerPixel currentWidth currentHeight (Vector 0 0 0) world background
                testingPicture (currentWidth + 1) currentHeight (resultedPixel : result) world background
                

runTest = 
    do 
        getWorld <- sceneWithPerlinShader
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1) "./foo.ppm"