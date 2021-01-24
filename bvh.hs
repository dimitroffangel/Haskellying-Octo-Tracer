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


-- construct BVH hittableList from end fronInterval toInterval chooseAxis 
--     |
