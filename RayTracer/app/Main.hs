module Main where

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

-- rotateCat0 = do
--   cat <- I.readImageRGB "./simple.jpg"
--   return cat

-- grad_color = I.makeImageR VU (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j))) / 400
-- test = I.writeImage "images/grad_color.png" grad_color


-- constants
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

-- auto material_ground = make_shared<lambertian>(color(0.8, 0.8, 0.0));
--     auto material_center = make_shared<lambertian>(color(0.7, 0.3, 0.3));
--     auto material_left   = make_shared<metal>(color(0.8, 0.8, 0.8));
--     auto material_right  = make_shared<metal>(color(0.8, 0.6, 0.2));

--     world.add(make_shared<sphere>(point3( 0.0, -100.5, -1.0), 100.0, material_ground));
--     world.add(make_shared<sphere>(point3( 0.0,    0.0, -1.0),   0.5, material_center));
--     world.add(make_shared<sphere>(point3(-1.0,    0.0, -1.0),   0.5, material_left));
--     world.add(make_shared<sphere>(point3( 1.0,    0.0, -1.0),   0.5, material_right));

sceneWithMetalMaterialAndLambertian = wrapInIO $
            HittableList [
                HittableGeometry $ Sphere (Vector 0 (-100.5) (-1)) 100 $ LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.8 0.8 0,
                HittableGeometry $ Sphere (Vector 0 0 (-1)) 0.5 $ LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.7 0.3 0.3,
                HittableGeometry $ Sphere (Vector (-1) 0 (-1)) 0.5 $ Metal (Vector 0.8 0.8 0.8) 0.3,
                HittableGeometry $ Sphere (Vector 1 0 (-1)) 0.5 $ Metal (Vector 0.8 0.6 0.2) 1
            ]

sceneWithMetalMaterialAndLambertianAndDielectric = wrapInIO $
        HittableList [
            HittableGeometry $ Sphere (Vector 0 (-100.5) (-1)) 100 $ LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.8 0.8 0,
            HittableGeometry $ Sphere (Vector 0 0 (-1)) 0.5 $ LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.7 0.3 0.3,
            HittableGeometry $ Sphere (Vector (-1) 0 (-1)) 0.5 $ Dielectric 1.5, 
            HittableGeometry $ Sphere (Vector 1 0 (-1)) 0.5 $ Metal (Vector 0.8 0.6 0.2) 0
        ]

randomScene = generateRandomScene (0) 4 (0) 4 []
sceneWithTextureOfSinusAndCosinus = generateSecondScene 
sceneWithPerlinShader = generateThirdScene
sceneWithPerlinTrilinearShader = generateThirdSceneWithTrilinearInterpolation
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





lookFrom = Vector 278 278 $ -800
-- lookFrom = Vector 26 3  6
-- lookFrom = Vector 13 2 3
-- lookAt = Vector 0 0 (0)
-- lookAt = Vector 0 2 0
lookAt = Vector 278 278 0
viewUp = Vector 0 1 0
distToFocus = 10
aperture = 0

-- mainCamera = constructCamera 90 aspectRatio (Vector (-2) 2 1) (Vector 0 0 (-1)) (Vector 0 1 0)
-- mainCamera = constructCamera 20 aspectRatio (Vector (-2) 2 1) (Vector 0 0 (-1)) (Vector 0 1 0)
-- mainCamera = constructCamera 20 aspectRatio aperture distToFocus lookFrom lookAt viewUp 0 1

randomSceneCamera = constructCamera 20 aspectRatio 0.1 10 (Vector 13 2 3) (Vector 0 0 0) viewUp 0 1
mainCamera = constructCamera 90 aspectRatio aperture distToFocus (Vector 0 0 0) (Vector 0 0 (-1)) viewUp 0 1 
-- (point3(-2,2,1), point3(0,0,-1), vec3(0,1,0), 20, aspect_ratio);
mainCameraSideWay = constructCamera 90 aspectRatio aperture distToFocus (Vector (-2) 2 1) (Vector 0 0 (-1)) viewUp 0 1
mainCameraZoomed = constructCamera 20 aspectRatio aperture distToFocus (Vector (-2) 2 1) (Vector 0 0 (-1)) viewUp 0 1

mainCamera1 = constructCamera 40 aspectRatio aperture distToFocus (Vector 13 2 3) (Vector 0 0 0) viewUp 0 1 
mainCamera2 = constructCamera 20 aspectRatio aperture distToFocus (Vector 26 3 6) (Vector 0 2 0) viewUp 0 1
mainCamera3 = constructCamera 40 1 aperture distToFocus lookFrom lookAt viewUp 0 1


-- mainCamera = depthOfFieldOfViewMainCamera

maxDepth = 10

transformEitherToIO :: Either a a -> IO a
transformEitherToIO (Right a) = return a
transformEitherToIO (Left a) = return a

-- linearly blends white and blue depending on y cooridnate of the unit vector of the ray direction
rayColour :: (Eq a, Num a) => Ray -> Vector -> HittableObject -> a -> IO Vector
rayColour ray@(Ray origin direction _) backgroundColour worldObjects 0 = wrapInIO $ Vector 0 0 0
rayColour ray@(Ray origin direction _) backgroundColour worldObjects depth =
    -- ignore hits close to zero
    let hitResult = hit worldObjects ray 0.001 infinity emptyHitRecord
        in case hitResult of 
            (Left _) -> wrapInIO backgroundColour
            (Right hit) -> 
                do 
                    -- using Lambertian reflection (hence all light is reflected equall-ish in all directions), 
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


shadePixel 0 _ _ resultedColour _ _ _ = wrapInIO resultedColour 
shadePixel index width height resultedColour world background camera =
    do
        randomGeneratedValueU <- generateNumberInInterval 0 1
        randomGeneratedValueV <- generateNumberInInterval 0 1
        unitDiskVector <- getRandomVectorInDiskCenter
        getRandomInitialStartOfRayCasting <- generateNumberInInterval (sendRayFromTime camera) (sendRayUntilTime camera) 
        unwrappedColour <- 
            let 
                u =  ((realToFrac width) + randomGeneratedValueU) / realToFrac (imageWidth - 1)
                v = ((realToFrac height) + randomGeneratedValueV) / realToFrac (imageHeight - 1)
                ray = getRay u v camera unitDiskVector getRandomInitialStartOfRayCasting
                in (rayColour ray background world maxDepth)
        shadePixel (index - 1) width height (resultedColour + unwrappedColour) world background camera

testingPicture currentWidth currentHeight result world background camera
    | currentWidth == imageWidth && currentHeight == 0 = wrapInIO $ Image imageWidth imageHeight $ splitListOnLists imageWidth $ reverse result
    | currentWidth == imageWidth = testingPicture 0 (currentHeight - 1) result world background camera
    | otherwise =  do
                -- sum the colour here then it will be averaged before it is printed
                -- antilising way
                resultedPixel <- shadePixel samplePerPixel currentWidth currentHeight (Vector 0 0 0) world background camera
                testingPicture (currentWidth + 1) currentHeight (resultedPixel : result) world background camera
                

runTest3 = 
    do 
        getWorld <- makeEmptyCornelScene
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background0 mainCamera3) "./foo.ppm"


runTestSimpleLightScene = 
    do 
        getWorld <- sceneWithSimpleLight
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background0 mainCamera2) "./foo.ppm"

runTheWorld = 
    do 
        getWorld <- sceneWithTextureImage "./earthmap.jpg"
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 mainCamera1) "./foo.ppm"

sceneWithTextureCat = 
    do 
        getWorld <- sceneWithTextureImage "./simple.jpg"
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 mainCamera1) "./foo.ppm"


sceneWithMarbleFloorAndSphere =    
    do 
        getWorld <- generateThirdSceneWithTrilinearInterpolation
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 mainCamera1) "./foo.ppm"

sceneWithPerlinTexture =    
    do 
        getWorld <- generateThirdScene
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 mainCamera1) "./foo.ppm"

generateSceneWithTextureOfSinusAndCosinus = 
    do 
        getWorld <- sceneWithTextureOfSinusAndCosinus
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 mainCamera1) "./foo.ppm"


generateSceneWithReflectionRefractionDieletricsAndMotionBlurr =
    do 
        getWorld <- randomScene
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 randomSceneCamera) "./foo.ppm"
        
generateRandomSceneWithMaterials =
    do 
        getWorld <- generateRandomSceneWithoutMotionBlur (0) 4 (0) 4 []
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 randomSceneCamera) "./foo.ppm"
        
generateSceneWithMetalAndLambertianStuff =
    do 
        getWorld <- sceneWithMetalMaterialAndLambertian
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 mainCamera) "./foo.ppm"

generateSceneWithMetalMaterialAndLambertianAndDielectric =
    do 
        getWorld <- sceneWithMetalMaterialAndLambertianAndDielectric
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 mainCamera) "./foo.ppm"

generateSceneWithMetalMaterialAndLambertianAndDielectricCameraSidelined =
    do 
        getWorld <- sceneWithMetalMaterialAndLambertianAndDielectric
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 mainCameraSideWay) "./foo.ppm"

generateSceneWithMetalMaterialAndLambertianAndDielectricCameraZoomed =
    do 
        getWorld <- sceneWithMetalMaterialAndLambertianAndDielectric
        saveImage (testingPicture 0 (imageHeight -1) [] getWorld background1 mainCameraZoomed) "./foo.ppm"
       
main = generateSceneWithReflectionRefractionDieletricsAndMotionBlurr


-- fooFunc =
--     do
--         coolImage <- readImage "./coolImage.jpg" :: IO (Either String (Graphics.Image.Image VS RGB Word8))
--         return $ displayImage coolImage