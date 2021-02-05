module HittableTypes where

import HitRecord
import Vector
import AxisAlignedBoundingBox

-- u v = (theta, psi)
-- theta - angle from the bottom pole
-- psi - angel arround the -x +z to x -z 
-- u = fi / 2pi
-- v = theta / pi
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
} | EmptyBVH deriving (Show, Read, Eq)

data XYRect = XYRect{
    getXYRectx0 :: Double,
    getXYRectx1 :: Double,
    getXYRecty0 :: Double,
    getXYRecty1 :: Double,
    getXYRectXYRectZ :: Double,
    getXYRectXYRectMaterial :: Material
} deriving (Show, Read, Eq)

data XZRect = XZRect{
    getXZRectx0 :: Double,
    getXZRectx1 :: Double,
    getXZRectz0 :: Double,
    getXZRectz1 :: Double,
    getXZRectY :: Double,
    getXZRectMaterial :: Material
} deriving (Show, Read, Eq)

data YZRect = YZRect{
    getYZRecty0 :: Double,
    getYZRecty1 :: Double,
    getYZRectz0 :: Double,
    getYZRectz1 :: Double,
    getYZRectX :: Double,
    getYZRectMaterial :: Material
} deriving (Show, Read, Eq)


data Box = Box{
    boxMin :: Vector,
    boxMax :: Vector,
    sides :: [HittableObject]
} deriving (Show, Read, Eq)

data TranslateObject = Translate {
    hittableObject :: HittableObject,
    offset :: Vector
} deriving (Show, Read, Eq)

data RotateAroundY = RotateAroundY {
    hittable :: HittableObject,
    sinTheta :: Double, -- angle between the rotation
    cosTheta :: Double, 
    rotatedBoundingBox :: AABB
}deriving (Show, Read, Eq)

data ConstantMedium = ConstantMedium {
    boundaryHittable :: HittableObject,
    constantMediumMaterial :: Material,
    negativeInversedDensity :: Double
} deriving (Show, Read, Eq)

thinBorderDistance = 0.0001

-- newtype HittableList = HittableList [GeometryObject]

data HittableObject = 
    HittableGeometry (GeometryObject) 
    |
    HittableList {
        list :: [HittableObject]
    } 
    | BVHHittable (BVH) 
    | XYRectHittable (XYRect) 
    | XZRectHittable (XZRect)
    | YZRectHittable (YZRect)
    | BoxHittable (Box)
    | HittableTranslate (TranslateObject)
    | RotateAroundYHittable (RotateAroundY)
    | ConstantMediumHittable (ConstantMedium)
    deriving (Show, Read, Eq)



