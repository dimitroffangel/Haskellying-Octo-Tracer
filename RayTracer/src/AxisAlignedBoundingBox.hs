module AxisAlignedBoundingBox where

import Ray
import Vector

data AABB = AABB {
    minimumPoint :: Vector,
    maximumPoint :: Vector
} | EmptyAABB deriving (Show, Read, Eq)

constructAABB pointA pointB = AABB pointA pointB


hitAABB (AABB (Vector xMinimum yMinimum zMinimum) (Vector xMaximum yMaximum zMaximum)) 
        (Ray (Vector xOrigin yOrigin zOrigin) (Vector xDirection yDirection zDirection) _) tMin tMax = 
    hitHelper xDirection xMinimum xOrigin xMaximum tMin tMax &&   
    hitHelper yDirection yMinimum yOrigin yMaximum tMin tMax &&
    hitHelper zDirection zMinimum zOrigin zMaximum tMin tMax
    where 
        calculateMaxMin t0 t1 tMin tMax 
            | t0 > tMin && t1 < tMax && t0 >= t1 = False
            | t0 > tMin && t1 < tMax = True
            | t0 > tMin && t1 >= tMax && t0 >= tMax = False
            | t0 > tMin && t1 >= tMax = True
            | t0 <= tMin && t1 < tMax && tMin >= t1 = False
            | t0 <= tMin && t1 < tMax = True
            | t0 <= tMin && t1 >= tMax && tMin >= tMax = False
            | otherwise = True 
        hitHelper axisDirection axisMinimum axisOrigin axisMaximum tMin tMax =
            let inverseD = 1  /  axisDirection
                t0 = (axisMinimum - axisOrigin) * inverseD
                t1 = (axisMaximum- axisOrigin) * inverseD
            in if inverseD < 0
                then calculateMaxMin t1 t0 tMin tMax
                else calculateMaxMin t0 t1 tMin tMax


makeSurroundingBox (AABB minLHS maxLHS) (AABB minRHS maxRHS) = 
    let minPoint = Vector 
                            (min (x minLHS) (x minRHS))
                            (min (y minLHS) (y minRHS))
                            (min (z minLHS) (z minRHS))
        maxPoint = Vector 
                            (max (x maxLHS) (x maxRHS))
                            (max (y maxLHS) (y maxRHS))
                            (max (z maxLHS) (z maxRHS))
        in AABB minPoint maxPoint
