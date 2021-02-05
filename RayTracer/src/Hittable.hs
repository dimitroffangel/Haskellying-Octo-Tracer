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
import XZRect
import YZRect
import IO
import Box

import Data.Word

import qualified Graphics.Image as I
import Graphics.Image.Processing (rotate180)
import qualified Graphics.Image.Interface as IM
import qualified Graphics.Image.Interface.Repa as R
import Graphics.Image.ColorSpace
import Graphics.Image.Types



clear hittableList = []

hit (HittableGeometry sphere@(Sphere sphereCenter sphereRadius sphereMaterial))  = hitSphere sphere
hit (HittableGeometry movingSphere@(MovingSphere sphereCenterInitially sphereCenterAfterTime sphereRadius sphereMaterial fromTime untilTime))  = hitMovingSphere movingSphere
hit (XYRectHittable xyRect@(XYRect x0 x1 y0 y1 z xyRectMaterial)) = hitXYRect xyRect
hit (XZRectHittable xzRect@(XZRect x0 x1 z0 z1 y xzRectMaterial)) = hitXZRect xzRect
hit (YZRectHittable yzRect@(YZRect y0 y1 z0 z1 x yzRectMaterial)) = hitYZRect yzRect
hit (HittableList hittableList) = hitList hittableList
hit (BoxHittable (Box minPoint maxPoint boxSides)) = hitList boxSides
hit (HittableTranslate translateHittable) = hitTranslation translateHittable
hit (ConstantMediumHittable medium) = hitConstantMedium medium
hitTranslation (Translate hittableObject offset) 
 ray@(Ray rayOrigin rayDirection rayTime) tMin tMax 
 hitRecord@(HitRecord hitRecordPoint hitRecordNormal hitRecordMaterial u v hitRecordT hitRecordFrontFace)= 
     let offsetRay = Ray (rayOrigin - offset) rayDirection rayTime
     in 
         case hit hittableObject offsetRay tMin tMax hitRecord of
            (Left _) -> Left hitRecord
            (Right (HitRecord resultPoint resultNormal resultMaterial resultU resultV resultT resultFrontFace)) -> 
                Right $ setFaceNormal 
                            (HitRecord (resultPoint + offset) resultNormal resultMaterial resultU resultV resultT resultFrontFace)
                            offsetRay resultNormal

                            
makeBoundingBox :: HittableObject -> Double -> Double -> AABB -> Either AABB AABB
makeBoundingBox (HittableGeometry 
                                movingSphere@(MovingSphere sphereCenterInitially sphereCenterAfterTime 
                                                            sphereRadius sphereMaterial fromTime untilTime)) = sphereMakeBoundingBox movingSphere

makeBoundingBox (HittableGeometry 
                                sphere@(Sphere sphereCenter sphereRadius sphereMaterial)) = movingSphereMakeBoundBox sphere

makeBoundingBox (XYRectHittable xyRect@(XYRect x0 x1 y0 y1 z xyRectMaterial)) = xyRectBoundingBox xyRect
makeBoundingBox (XZRectHittable xzRect@(XZRect x0 x1 z0 z1 y xzRectMaterial)) = xzRectBoundingBox xzRect
makeBoundingBox (YZRectHittable yzRect@(YZRect y0 y1 z0 z1 x yzRectMaterial)) = yzRectBoundingBox yzRect
makeBoundingBox (HittableList hittableList) = hitListBoundingBox hittableList

-- makeBoundingBox :: HittableObject -> Double -> Double -> AABB -> Either AABB AABB
makeBoundingBox (BoxHittable box@(Box minPoint maxPoint boxSides)) = makeBoundingBoxFromBox box
makeBoundingBox (HittableTranslate hittableTranslate) = makeBoundingBoxFromTranslatedBox hittableTranslate

makeBoundingBoxFromTranslatedBox (Translate hittable offset) fromInterval toInterval result =
        let resultAABB = makeBoundingBox hittable fromInterval toInterval result
        in case resultAABB of 
            (Left _) -> Left result
            (Right (AABB min max)) -> Right $ AABB (min + offset) (max + offset)


hitConstantMedium (ConstantMedium hittable material negativeDensity) ray@(Ray rayOrigin rayDirection _) tMin tMax hitRecord =
    let hitRecord1 = hit hittable ray (-infinity) infinity emptyHitRecord
    in case hitRecord1 of
            (Left _) -> Left hitRecord
            (Right result1) ->
                let hitRecord2 = hit hittable ray (t result1) infinity emptyHitRecord
                in case hitRecord2 of
                    (Left _) -> Left hitRecord
                    (Right result2) -> 
                            hitConstantMediumHelper result1 result2
    where hitConstantMediumHelper 
            result1@(HitRecord resultPoint1 resultNormal1 resultMaterial1 resultU1 resultV1 resultT1 resultHitRecordFrontFace1)
            result2@(HitRecord resultPoint2 resultNormal2 resultMaterial2 resultU2 resultV2 resultT2 resultHitRecordFrontFace2)
            | resultT1 < tMin = 
                hitConstantMediumHelper 
                    (HitRecord resultPoint1 resultNormal1 resultMaterial1 resultU1 resultV1 tMin resultHitRecordFrontFace1)
                    result2
            | resultT2 > tMax= 
                hitConstantMediumHelper 
                    result1
                    (HitRecord resultPoint2 resultNormal2 resultMaterial2 resultU2 resultV2 tMax resultHitRecordFrontFace2)
            | resultT1 >= resultT2 = Left hitRecord
            | resultT1 < 0 =
                hitConstantMediumHelper 
                    (HitRecord resultPoint1 resultNormal1 resultMaterial1 resultU1 resultV1 0 resultHitRecordFrontFace1)
                    result2
            | otherwise = 
                let rayLength = getVectorLength rayDirection
                    distanceInsideBoundary = (resultT2 - resultT1) * rayLength
                    hitDistance = negativeDensity * log 0.23
                    in if hitDistance > distanceInsideBoundary 
                        then Left hitRecord
                        else 
                            let newT = resultT1 + hitDistance / rayLength
                            in Right $ HitRecord (getPointLocation ray newT) (Vector 1 0 0) material  resultU1 resultV1 newT True

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

bvhHit (BVH leftSide rightSide box) ray tMin tMax hitRecord 
    | not $ hitAABB box ray tMin tMax = Left hitRecord
    | otherwise = 
        let leftHit = hit leftSide ray tMin tMax hitRecord
            in case leftHit of
                (Left _) -> hit rightSide ray tMin tMax hitRecord
                (Right newHitRecord) -> hit rightSide ray tMin (t newHitRecord) hitRecord

bvhBoundingBox (BVH leftSide rightSide box) fromInterval toInterval currentBox = Right box

constructBVH [hittable] from end fromInterval toInterval=
    let boundingBox = makeBoundingBox hittable fromInterval toInterval EmptyAABB
    in case boundingBox of 
        (Left boundingBox) -> wrapInIO $ Right $ BVH hittable hittable $ makeSurroundingBox boundingBox boundingBox
        (Right boundingBox) -> wrapInIO $ Right $ BVH hittable hittable $ makeSurroundingBox boundingBox boundingBox

constructBVH [hittable1, hittable2] from end fromInterval toInterval =
    do 
        getRandomAxis <- generateIntegerInInterval 0 2
        if boxCompare getRandomAxis hittable1 hittable2
            then constructBVH [hittable2, hittable1] from end fromInterval toInterval
            else 
                let
                    leftBoundingBox = makeBoundingBox hittable2 fromInterval toInterval EmptyAABB
                    rightBoundingBox = makeBoundingBox hittable1 fromInterval toInterval EmptyAABB
                    in case leftBoundingBox of
                        (Left _) -> return $ Left EmptyBVH
                        (Right leftBox) ->
                            case rightBoundingBox of 
                                (Left _) -> return $ Left EmptyBVH
                                (Right rightBox) -> return $ Right $ BVH hittable2 hittable1 $ makeSurroundingBox leftBox rightBox 
constructBVH hittableList from end fromInterval toInterval= 
    do 
        getRandomAxis <- generateIntegerInInterval 0 2
        let 
            sortedList = quicksort hittableList (boxCompare getRandomAxis)
            middle = from + (end - from) `div` 2
            in do
                leftTree <- constructBVH (take middle sortedList) from middle fromInterval toInterval
                case leftTree of 
                    (Left _) -> return $ Left EmptyBVH
                    (Right leftTreeBVH) ->
                        do 
                            rightTree <- constructBVH (drop middle sortedList) middle end fromInterval toInterval
                            case rightTree of 
                                (Left _) -> return $ Left EmptyBVH
                                (Right rightTreeBVH) ->
                                    return $ Right $ BVH (BVHHittable leftTreeBVH) (BVHHittable rightTreeBVH) 
                                        $ makeSurroundingBox (bvhBox leftTreeBVH) $ bvhBox rightTreeBVH

boxCompare axis lhsHittable rhsHittable  
    | axis == 0 = 
        let xLHSMin = x $ minimumPoint $ 
                case makeBoundingBox lhsHittable 0 0 EmptyAABB of 
                (Left res) -> res 
                (Right res) -> res 
            xRHSMin = x $ minimumPoint $ 
                case makeBoundingBox rhsHittable 0 0 EmptyAABB of
                    (Left res) -> res
                    (Right res) -> res
            in xLHSMin < xRHSMin
    | axis == 1 = 
        let yLHSMin = y $ minimumPoint $  
                case makeBoundingBox lhsHittable 0 0 EmptyAABB of 
                (Left res) -> res 
                (Right res) -> res             
            yRHSMin = y $ minimumPoint $                 
                case makeBoundingBox rhsHittable 0 0 EmptyAABB of
                (Left res) -> res
                (Right res) -> res
            in yLHSMin < yRHSMin
    | axis == 2 = 
        let zLHSMin = z $ minimumPoint $  
                case makeBoundingBox lhsHittable 0 0 EmptyAABB of 
                    (Left res) -> res 
                    (Right res) -> res 
            zRHSMin = z $ minimumPoint $                
                    case makeBoundingBox rhsHittable 0 0 EmptyAABB of
                    (Left res) -> res
                    (Right res) -> res
            in zLHSMin < zRHSMin

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
 
generateRandomSceneWithoutMotionBlur xIndex maxXIndex yIndex maxYIndex result 
    | xIndex == maxXIndex && yIndex == maxYIndex - 1 = 
        wrapInIO $ HittableList $ 
                (HittableGeometry $ Sphere (Vector 0 (-1000) 0) 1000 groundMaterial) :
                (HittableGeometry $ Sphere (Vector 0 1 0) 1 $ Dielectric 1.5) : 
                (HittableGeometry $ Sphere (Vector (-4) 1 0) 1 $ LambertianMaterial $ (SolidColourTexture $ SolidColour $ Vector 0.4 0.2 0.1)) : 
                (HittableGeometry $ Sphere (Vector 4 1 0) 1 $ Metal (Vector 0.7 0.6 0.5) 0) : (reverse result)
    | xIndex == maxXIndex = generateRandomSceneWithoutMotionBlur (0) maxXIndex (yIndex + 1) maxYIndex result
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
                        then generateRandomSceneWithoutMotionBlur (xIndex + 1) maxXIndex yIndex maxYIndex result
                        else produceObject pointCenter centerAfterMoving materialCoefficient xRandom zRandom randomVectorColour 
                                    randomVectorColour2 metalColour metalFuzziness
                            where
                                produceObject pointCenter centerAfterMoving materialCoefficient xRandom zRandom randomVectorColour 
                                    randomVectorColour2 metalColour metalFuzziness
                                 -- lambertian  
                                    | (materialCoefficient < 0.8) = 
                                        generateRandomSceneWithoutMotionBlur (xIndex + 1) maxXIndex yIndex maxYIndex 
                                            ((HittableGeometry $ Sphere pointCenter 0.2 
                                                                (LambertianMaterial $ SolidColourTexture $ SolidColour 
                                                                $ randomVectorColour * randomVectorColour2)) : result) 
                                -- metal
                                    | (materialCoefficient < 0.95) =  generateRandomSceneWithoutMotionBlur (xIndex + 1) maxXIndex yIndex maxYIndex 
                                        ((HittableGeometry $ Sphere pointCenter 0.2 $ Metal metalColour metalFuzziness) : result)
                                    | otherwise = generateRandomSceneWithoutMotionBlur (xIndex + 1) maxXIndex yIndex maxYIndex 
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
                    HittableGeometry $ Sphere (Vector 0 (-1000) 0) 1000 (LambertianMaterial $ MarbleTexture $ TrilinearNoise pertexMaterial1 4),
                    HittableGeometry $ Sphere (Vector 0 2 0) 2 (LambertianMaterial $ MarbleTexture $ TrilinearNoise pertexMaterial1 4)
                ]            
            

fooMaterial = DiffuseLight $ SolidColourTexture $ SolidColour $ Vector 10 10 10

frog = I.readImage "./simple.jpg" :: IO (Either String (I.Image VS RGB Double))

loadJPGImage filePath= 
    do 
        testFrog <- I.readImage filePath :: IO (Either String (I.Image VS RGB Double))
        case testFrog of 
            (Left err)-> return $ Image 0 0 [[Vector 1 2 3]]
            (Right res)-> 
                let rgbList = I.toLists res
                    in return $ Image (I.cols res) (I.rows res) $ map (\row -> map (\(PixelRGB a b c) -> Vector a b c) row) rgbList


sceneWithTextureImage filePath = 
    do 
        image <- loadJPGImage filePath
        return $ HittableList  
            [
               HittableGeometry $ Sphere (Vector 0 0 0) 2.5 $ LambertianMaterial $ ImageTexture (content image) (width image) (height image)
            ]

sceneWithSimpleLight = 
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
            in return $ 
                    HittableList $
                        (HittableGeometry $ Sphere (Vector 0 7 0) 2 fooMaterial) :
                        (XYRectHittable $ XYRect 3 5 1 3 (-2) fooMaterial)  : 
                        [
                            (HittableGeometry $ Sphere (Vector 0 (2) 0) 2 
                                (LambertianMaterial $ MarbleTexture $ TrilinearNoise pertexMaterial1 4)),
                            (HittableGeometry $ Sphere (Vector 0 (-1000) 0) 1000 
                                (LambertianMaterial $ MarbleTexture $ TrilinearNoise pertexMaterial1 4))
                        ] 


red   = LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.65 0.05 0.05
white = LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.73 0.73 0.73
green = LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.12 0.45 0.15
light = DiffuseLight $ SolidColourTexture $ SolidColour $ Vector 30 30 30

makeEmptyCornelScene =
        wrapInIO $ HittableList  
            [
                YZRectHittable $ YZRect 0 555 0 555 555 green,
                YZRectHittable $ YZRect 0 555 0 555 0 red,
                XZRectHittable $ XZRect 213 343 227 332 554 light,
                XZRectHittable $ XZRect 0 555 0 555 0 white,
                XZRectHittable $ XZRect 0 555 0 555 555 white,
                XYRectHittable $ XYRect 0 555 0 555 555 white,
                BoxHittable $ constructBox (Vector 130 0 65) (Vector 295 165 230) white,
                BoxHittable $ constructBox (Vector 265 0 295) (Vector 430 330 460) white
            ]



box1 = HittableTranslate $ Translate (BoxHittable $ constructBox (Vector 0 0 0) (Vector 165 330 165) white) $ Vector 265 0 295
box2 =  HittableTranslate $ Translate (BoxHittable $ constructBox (Vector 0 0 295) (Vector 430 330 460) white) $ Vector 130 0 65
makeSmokeCornelBox =
        wrapInIO $ HittableList  
            [
                YZRectHittable $ YZRect 0 555 0 555 555 green,
                YZRectHittable $ YZRect 0 555 0 555 0 red,
                XZRectHittable $ XZRect 113 443 127 432 554 light,
                XZRectHittable $ XZRect 0 555 0 555 555 white,
                XZRectHittable $ XZRect 0 555 0 555 0 white,
                XYRectHittable $ XYRect 0 555 0 555 555 white,
                box1,
                box2,
                ConstantMediumHittable $ ConstantMedium box1 (LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 0.65 0.05 0.05) 0.01,
                ConstantMediumHittable $ ConstantMedium box2 (LambertianMaterial $ SolidColourTexture $ SolidColour $ Vector 1 1 1) 0.01
            ]




















