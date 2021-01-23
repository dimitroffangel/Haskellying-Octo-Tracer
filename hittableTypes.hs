module HittableTypes where

import HitRecord
import Vector

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
}deriving (Show, Read, Eq)