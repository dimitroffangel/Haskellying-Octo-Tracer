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


-- newtype HittableList = HittableList [Sphere]

clear hittableList = []

hit sphere@(Sphere sphereCenter sphereRadius sphereMaterial)  = hitSphere sphere
hit movingSphere@(MovingSphere sphereCenterInitially sphereCenterAfterTime sphereRadius sphereMaterial fromTime untilTime)  = hitMovingSphere movingSphere

makeBoundingBox movingSphere@(MovingSphere sphereCenterInitially sphereCenterAfterTime sphereRadius sphereMaterial fromTime untilTime) = sphereMakeBoundingBox movingSphere
makeBoundingBox sphere@(Sphere sphereCenter sphereRadius sphereMaterial) = movingSphereMakeBoundBox sphere


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
            let newBoundingBox = makeBoundingBox currentObject fromInterval toInterval 
            in case newBoundingBox of 
                -- if constructing a bounding box for the current object has failed return the currentBox
                (Left _) -> Left resultBox
                -- bind the newly-formed bounding box with the currentlyFound
                (Right box) -> hitListBoundingBoxHelper restOfList fromInterval toInterval (makeSurroundingBox box resultBox) True

            
              
groundMaterial = LambertianMaterial $ Vector 0.5 0.5 0.5

generateRandomScene xIndex maxXIndex yIndex maxYIndex result 
    | xIndex == maxXIndex && yIndex == maxYIndex - 1 = 
        wrapInIO $ 
                (Sphere (Vector 0 (-1000) 0) 1000 groundMaterial) :
                (Sphere (Vector 0 1 0) 1 $ Dielectric 1.5) : 
                (Sphere (Vector (-4) 1 0) 1 $ LambertianMaterial $ Vector 0.4 0.2 0.1) : 
                (Sphere (Vector 4 1 0) 1 $ Metal (Vector 0.7 0.6 0.5) 0) : (reverse result)
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
                        else produceObject pointCenter centerAfterMoving materialCoefficient xRandom zRandom randomVectorColour randomVectorColour2 metalColour metalFuzziness
                            where
                                produceObject pointCenter centerAfterMoving materialCoefficient xRandom zRandom randomVectorColour randomVectorColour2 metalColour metalFuzziness
                                 -- lambertian  
                                    | (materialCoefficient < 0.8) = 
                                        generateRandomScene (xIndex + 1) maxXIndex yIndex maxYIndex 
                                            ((MovingSphere pointCenter centerAfterMoving 0.2 (LambertianMaterial $ randomVectorColour * randomVectorColour2) 0 1) : result) 
                                -- metal
                                    | (materialCoefficient < 0.95) =  generateRandomScene (xIndex + 1) maxXIndex yIndex maxYIndex ((Sphere pointCenter 0.2 $ Metal metalColour metalFuzziness) 
                                        : result)
                                    | otherwise = generateRandomScene (xIndex + 1) maxXIndex yIndex maxYIndex ((Sphere pointCenter 0.2 $ Dielectric 1.5) : result)
                           