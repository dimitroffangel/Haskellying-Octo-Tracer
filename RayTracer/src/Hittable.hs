module Hittable where

import Vector
import Ray
import HitRecord
import HittableTypes
import Sphere
import MovingSphere
import GeneratingRandomStuff
import UtilityFunctions
import AxisAlignedBoundingBox
import Texture
import PerlinShade
import XYRect


clear hittableList = []

hit (HittableGeometry sphere@(Sphere sphereCenter sphereRadius sphereMaterial))  = hitSphere sphere
hit (HittableGeometry movingSphere@(MovingSphere sphereCenterInitially sphereCenterAfterTime sphereRadius sphereMaterial fromTime untilTime))  = hitMovingSphere movingSphere
hit (XYRectHittable xyRect@(XYRect x0 x1 y0 y1 z xyRectMaterial)) = hitXYRect xyRect
hit (HittableList hittableList) = hitList hittableList

makeBoundingBox (HittableGeometry 
                                movingSphere@(MovingSphere sphereCenterInitially sphereCenterAfterTime 
                                                            sphereRadius sphereMaterial fromTime untilTime)) = sphereMakeBoundingBox movingSphere

makeBoundingBox (HittableGeometry 
                                sphere@(Sphere sphereCenter sphereRadius sphereMaterial)) = movingSphereMakeBoundBox sphere

makeBoundingBox (XYRectHittable xyRect@(XYRect x0 x1 y0 y1 z xyRectMaterial)) = xyRectBoundingBox xyRect
makeBoundingBox (HittableList hittableList) = hitListBoundingBox hittableList

hitList hittableList ray@(Ray rayOrigin rayDirection _) tMin tMax hitRecord =
    hitHelper hittableList tMax hitRecord False
        where 
            hitHelper [] _ tempRecord True = Right tempRecord 
            hitHelper [] _ tempRecord False = Left tempRecord
            hitHelper (currentHittable : rest) closestT tempHitRecord hasHit =
                let newHit = hit currentHittable ray tMin closestT tempHitRecord 
                in case newHit of 
                    (Left _) -> hitHelper rest closestT tempHitRecord hasHit
                    (Right resultHit) -> hitHelper rest (t resultHit) resultHit True

-- construct the boundingBox for a list of geometryObjects
hitListBoundingBox hittableList fromInterval toInterval currentBox =
    hitListBoundingBoxHelper hittableList fromInterval toInterval currentBox False
    where
        -- has reached the ened return the result
        hitListBoundingBoxHelper [] _ _ resultBox True = Right resultBox
        -- the list is empty from the get go
        hitListBoundingBoxHelper [] _ _ resultBox False = Left resultBox
        hitListBoundingBoxHelper (currentObject : restOfList) fromInterval toInterval resultBox hasNotChangedBox =
            let newBoundingBox = makeBoundingBox currentObject fromInterval toInterval resultBox 
            in case newBoundingBox of 
                -- if constructing a bounding box for the current object has failed return the currentBox
                (Left _) -> Left resultBox
                -- bind the newly-formed bounding box with the currentlyFound
                (Right box) -> hitListBoundingBoxHelper restOfList fromInterval toInterval (makeSurroundingBox box resultBox) True

            
              
groundMaterial = LambertianMaterial $ 
                        SimpleTexture (SolidColourTexture $ SolidColour $ Vector 0.2 0.3 0.1) 
                                      (SolidColourTexture $ SolidColour $ Vector 0.9 0.9 0.9)


-- groundMaterial = LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.5 0.5 0.5


generateRandomScene xIndex maxXIndex yIndex maxYIndex result 
    | xIndex == maxXIndex && yIndex == maxYIndex - 1 = 
        wrapInIO $ HittableList $ 
                (HittableGeometry $ Sphere (Vector 0 (-1000) 0) 1000 groundMaterial) :
                (HittableGeometry $ Sphere (Vector 0 1 0) 1 $ Dielectric 1.5) : 
                (HittableGeometry $ Sphere (Vector (-4) 1 0) 1 $ LambertianMaterial $ (SolidColourTexture $ SolidColour $ Vector 0.4 0.2 0.1)) : 
                (HittableGeometry $ Sphere (Vector 4 1 0) 1 $ Metal (Vector 0.7 0.6 0.5) 0) : (reverse result)
    | xIndex == maxXIndex = generateRandomScene (0) maxXIndex (yIndex + 1) maxYIndex result
    | otherwise = 
        do 
            materialCoefficient <- generateNumberInInterval 0 1
            xRandom <- generateNumberInInterval 0 1
            yCenterAfterMoving <- generateNumberInInterval 0 0.5
            zRandom <- generateNumberInInterval 0 1
            randomVectorColour <- getRandomVectorInInterval 0 1
            randomVectorColour2 <- getRandomVectorInInterval 0 1
            metalColour <- getRandomVectorInInterval 0.5 1
            metalFuzziness <- generateNumberInInterval 0 0.5
            let pointCenter = Vector (xIndex + 0.9 *xRandom) 0.2 (yIndex + 0.9 * zRandom) 
                centerAfterMoving = pointCenter + (Vector 0 yCenterAfterMoving 0)
                in if getSquaredVector (pointCenter - (Vector 4 0.2 0)) < 0.81
                        then generateRandomScene (xIndex + 1) maxXIndex yIndex maxYIndex result
                        else produceObject pointCenter centerAfterMoving materialCoefficient xRandom zRandom randomVectorColour 
                                    randomVectorColour2 metalColour metalFuzziness
                            where
                                produceObject pointCenter centerAfterMoving materialCoefficient xRandom zRandom randomVectorColour 
                                    randomVectorColour2 metalColour metalFuzziness
                                 -- lambertian  
                                    | (materialCoefficient < 0.8) = 
                                        generateRandomScene (xIndex + 1) maxXIndex yIndex maxYIndex 
                                            ((HittableGeometry $ MovingSphere pointCenter centerAfterMoving 0.2 
                                                                (LambertianMaterial $ SolidColourTexture $ SolidColour 
                                                                $ randomVectorColour * randomVectorColour2) 0 1) : result) 
                                -- metal
                                    | (materialCoefficient < 0.95) =  generateRandomScene (xIndex + 1) maxXIndex yIndex maxYIndex 
                                        ((HittableGeometry $ Sphere pointCenter 0.2 $ Metal metalColour metalFuzziness) : result)
                                    | otherwise = generateRandomScene (xIndex + 1) maxXIndex yIndex maxYIndex 
                                        ((HittableGeometry $ Sphere pointCenter 0.2 $ Dielectric 1.5) : result)
                           

generateSecondScene =  wrapInIO $ 
                                    HittableList $ [
                                                    (HittableGeometry $ Sphere (Vector 0 (-10) 0) 10 groundMaterial),
                                                    (HittableGeometry $ Sphere (Vector 0 (10) 0) 10 groundMaterial)
                                                ]


generateRandomUnitVecctorsForNoise1 = transformListOfIOToIOOfList [ getUnitVector <$> getRandomVectorInInterval (-1) 1 | x <- [1..pointCounter]]
generateRandomNumbersForNoise1 =  transformListOfIOToIOOfList [ (generateNumberInInterval 0 1) | x <- [1..pointCounter]]
generateRandomNumbersForNoise2 =  transformListOfIOToIOOfList [ (generateNumberInInterval 0 1) | x <- [1..pointCounter]]
generateRandomNumbersForNoiseX1 = transformListOfIOToIOOfList  [(generateIntegerInInterval 0 (pointCounter - x)) | x <- [1..pointCounter]]
generateRandomNumbersForNoiseX2 = transformListOfIOToIOOfList  [(generateIntegerInInterval 0 (pointCounter - x)) | x <- [1..pointCounter]]
generateRandomNumbersForNoiseY1 = transformListOfIOToIOOfList  [(generateIntegerInInterval 0 (pointCounter - x)) | x <- [1..pointCounter]]
generateRandomNumbersForNoiseY2 = transformListOfIOToIOOfList  [(generateIntegerInInterval 0 (pointCounter - x)) | x <- [1..pointCounter]]
generateRandomNumbersForNoiseZ1 = transformListOfIOToIOOfList  [(generateIntegerInInterval 0 (pointCounter - x)) | x <- [1..pointCounter]]
generateRandomNumbersForNoiseZ2 = transformListOfIOToIOOfList  [(generateIntegerInInterval 0 (pointCounter - x)) | x <- [1..pointCounter]]

transformListOfIOToIOOfList list= 
    foldr (\el arr -> 
        do 
            unwrapEl <- el
            unwrapARr <- arr
            return $ unwrapEl : unwrapARr
        ) (wrapInIO []) list
                          

generateThirdScene =  
    do
        giveRandomUnitVectosForNoise1 <- generateRandomUnitVecctorsForNoise1
        giveRandomNumbersForNoise1  <- generateRandomNumbersForNoise1 
        giveRandomNumbersForNoise2  <- generateRandomNumbersForNoise2 
        giveRandomNumbersForNoiseX1 <- generateRandomNumbersForNoiseX1
        giveRandomNumbersForNoiseX2 <- generateRandomNumbersForNoiseX2
        giveRandomNumbersForNoiseY1 <- generateRandomNumbersForNoiseY1
        giveRandomNumbersForNoiseY2 <- generateRandomNumbersForNoiseY2
        giveRandomNumbersForNoiseZ1 <- generateRandomNumbersForNoiseZ1
        giveRandomNumbersForNoiseZ2 <- generateRandomNumbersForNoiseZ2
        let 
            pertexMaterial1 = constructPerlinShader giveRandomUnitVectosForNoise1
                                                    giveRandomNumbersForNoise1 
                                                    giveRandomNumbersForNoiseX1 
                                                    giveRandomNumbersForNoiseY1 
                                                    giveRandomNumbersForNoiseZ1                            
            -- pertexMaterial2 = constructPerlinShader giveRandomNumbersForNoise2 
            --                                         giveRandomNumbersForNoiseX2 
            --                                         giveRandomNumbersForNoiseY2 
            --                                         giveRandomNumbersForNoiseZ2
            in return $ HittableList [
                    HittableGeometry $ Sphere (Vector 0 (-1000) 0) 1000 (LambertianMaterial $ NoiseTexture $ Noise $ pertexMaterial1),
                    HittableGeometry $ Sphere (Vector 0 2 0) 2 (LambertianMaterial $ NoiseTexture $ Noise pertexMaterial1)
                ]            
            
generateThirdSceneWithTrilinearInterpolation =  
    do
        giveRandomUnitVectosForNoise1 <- generateRandomUnitVecctorsForNoise1
        giveRandomNumbersForNoise1  <- generateRandomNumbersForNoise1 
        giveRandomNumbersForNoise2  <- generateRandomNumbersForNoise2 
        giveRandomNumbersForNoiseX1 <- generateRandomNumbersForNoiseX1
        giveRandomNumbersForNoiseX2 <- generateRandomNumbersForNoiseX2
        giveRandomNumbersForNoiseY1 <- generateRandomNumbersForNoiseY1
        giveRandomNumbersForNoiseY2 <- generateRandomNumbersForNoiseY2
        giveRandomNumbersForNoiseZ1 <- generateRandomNumbersForNoiseZ1
        giveRandomNumbersForNoiseZ2 <- generateRandomNumbersForNoiseZ2
        let 
            pertexMaterial1 = constructPerlinShader giveRandomUnitVectosForNoise1
                                                    giveRandomNumbersForNoise1 
                                                    giveRandomNumbersForNoiseX1 
                                                    giveRandomNumbersForNoiseY1 
                                                    giveRandomNumbersForNoiseZ1                            
            -- pertexMaterial2 = constructPerlinShader giveRandomNumbersForNoise2 
            --                                         giveRandomNumbersForNoiseX2 
            --                                         giveRandomNumbersForNoiseY2 
            --                                         giveRandomNumbersForNoiseZ2
            in return $ HittableList [
                    HittableGeometry $ Sphere (Vector 0 (-1000) 0) 1000 (LambertianMaterial $ TrilinearNoiseTexture $ TrilinearNoise pertexMaterial1 4),
                    HittableGeometry $ Sphere (Vector 0 2 0) 2 (LambertianMaterial $ TrilinearNoiseTexture $ TrilinearNoise pertexMaterial1 4)
                ]            
            

fooMaterial = DiffuseLight $ SolidColourTexture $ SolidColour $ Vector 4 4 4

sceneWithSimpleLight = wrapInIO $ 
        HittableList $
            (HittableGeometry $ Sphere (Vector 0 7 0) 2 fooMaterial) :
            (XYRectHittable $ XYRect 3 5 1 3 (-2) fooMaterial)  : 
            [
                (HittableGeometry $ Sphere (Vector 0 (2) 0) 2 (LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.8 0.8 0)),
                (HittableGeometry $ Sphere (Vector 0 (-1000) 0) 1000 (LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.8 0.8 0))
            ] 