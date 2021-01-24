module BVH where

import Hittable
import Ray

bvhHit (BVH leftSide rightSide box) ray tMin tMax hitRecord 
    | not $ hitAABB box ray tMin tMax = Left hitRecord
    | otherwise = 
        let leftHit = hit leftSide ray tMin tMax hitRecord
            in case leftHit of
                (Left _) -> hit rightSide ray tMin tMax hitRecod
                (Right newHitRecord) -> hit rightSide tMin (t newHitRecord) hitRecord



constructBVH [hittable] from end fromInterval toInterval=
    let boundingBox = (makeBoundingBox hittable)
    in BVH hittable hittable $ makeSurroundingBox  boundingBox boundingBox
constructBVH [hittable1 hittable2] from end fromInterval toInterval =
    do 
        getRandomAxis <- generateIntegerInInterval (0 2)
        if boxCompare hittable1 hittable 2 getRandomAxis
            then return $ BVH hittable1 hittable2 $ makeSurroundingBox (makeBoundingBox hittable1) (makeBoundingBox hittable2)
            else return $ BVH hittable2 hittable1 $ makeSurroundingBox (makeBoundingBox hittable2) (makeBoundingBox hittable1) 
constructBVH hittableList from end fronInterval toInterval= 
    do 
        getRandomAxis <- generateIntegerInInterval (0 2)
        return $ 
            let 
                sortedList = quicksort hittableList boxCompare
                middle = from + (end - from) / 2
                leftTree = constructBVH (take middle sortedList) from middle fromInterval toInterval   
                rightTree = constructBVH (drop middle sortedList) middle end fromInterval toInterval   
                in BVH leftTree rightTree $ makeSurroungingBox (makeBoundingBox leftTree) (makeBoundingBox rightTree)


boxCompare lhsHittable rhsHittable axis 
    | axis == 0 = 
        let xLHSMin = x $ makeBoundingBox lhsHittable 0 0 EmptyAABB
            xRHSMin = x $ makeBoundingBox rhsHittable 0 0 EmptyAABB
            in xLHSMin < xRHSMin
    | axis == 1 = 
        let yLHSMin = y $ makeBoundingBox lhsHittable 0 0 EmptyAABB
            yRHSMin = y $ makeBoundingBox rhsHittable 0 0 EmptyAABB
            in yLHSMin < yRHSMin
    | axis == 2 = 
        let zLHSMin = z $ makeBoundingBox lhsHittable 0 0 EmptyAABB
            zRHSMin = z $ makeBoundingBox rhsHittable 0 0 EmptyAABB
            in zLHSMin < zRHSMin