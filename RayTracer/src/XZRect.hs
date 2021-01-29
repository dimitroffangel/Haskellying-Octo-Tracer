module XYRect where

import AxisAlignedBoundingBox
import HitRecord
import Ray
import Vector
import HittableTypes


xzRectBoundingBox (XZRect x0 x1 z0 z1 y _) fromInterval toInterval result =
    Right $ AABB (Vector x0 (y - thinBorderDistance) z0) $ Vector x1 (y + thinBorderDistance) z1 

