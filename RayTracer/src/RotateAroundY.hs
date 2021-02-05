module RotateAroundY where

import HittableTypes
import AxisAlignedBoundingBox
import UtilityFunctions

-- constructRotationAroundY hittable angle =
--     let radians = degreesToRadians angle
--         sinTheta = sin radians
--         cosTheta = cos radians
--         boundingBox = makeBoundingBox hittable 0 1 EmptyAABB
--         in 