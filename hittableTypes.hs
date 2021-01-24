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

data XYRect = XYRect{
    x0 :: Double,
    x1 :: Double,
    y0 :: Double,
    y1 :: Double,
    xyRectZ :: Double,
    xyRectMaterial :: Material
} deriving (Show, Read, Eq)

-- newtype HittableList = HittableList [GeometryObject]

data HittableObject = 
    HittableGeometry (GeometryObject) 
    |
    HittableList {
        list :: [HittableObject]
    } 
    | BVHHittable (BVH) 
    | XYRectHittable (XYRect)
    deriving (Show, Read, Eq)



