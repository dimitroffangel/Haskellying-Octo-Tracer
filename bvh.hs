module BVH where

import Hittable
import Ray

data BVH = BVH {
  leftSideHittableObjects :: HittableList,
  rightSideHittableObjects :: HittableList
  bvhBox :: AABB
} deriving (show, read, Eq)


bvhHit (BVH leftSide rightSide box) ray tMin tMax hitRecord 
    | not $ hitAABB box ray tMin tMax = Left hitRecord
    | otherwise = 
        let leftHit = hitList leftSide ray tMin tMax hitRecord
            in case leftHit of
                (Left _) -> hitList rightSide ray tMin tMax hitRecod
                (Right newHitRecord) -> hitList rightSide tMin (t newHitRecord) hitRecord
