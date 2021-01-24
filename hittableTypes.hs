module HittableTypes where

import HitRecord
import Vector
import AxisAlignedBoundingBox


data GeometryObject = Sphere {
    sphereCenter :: Vector,
    sphereRadius :: Double,
    sphereMaterial :: Material
} | MovingSphere {
    sphereInitiallyCenter :: Vector,
    sphereCenterAfterTime :: Vector,
    sphereRadius :: Double,
    sphereMaterial :: Material,
    movingFromTime :: Double,
    movingUntilTime :: Double
} deriving (Show, Read, Eq)


data BVH = BVH {
  leftSideHittableObjects :: HittableObject,
  rightSideHittableObjects :: HittableObject,
  bvhBox :: AABB
} deriving (Show, Read, Eq)


-- newtype HittableList = HittableList [GeometryObject]

data HittableObject = 
    HittableGeometry (GeometryObject) 
    |
    HittableList {
        list :: [HittableObject]
    } 
    | BVHHittable (BVH) 
    deriving (Show, Read, Eq)



