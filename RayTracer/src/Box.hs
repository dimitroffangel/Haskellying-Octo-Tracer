module Box where

import HittableTypes
import Vector
import AxisAlignedBoundingBox

constructBox minPoint@(Vector x0 y0 z0) maxPoint@(Vector x1 y1 z1) material =
    let listOfHittables = 
             [
                XYRectHittable $ XYRect x0 x1 y0 y1 z1 material,
                XYRectHittable $ XYRect x0 x1 y0 y1 z0 material,
                XZRectHittable $ XZRect x0 x1 z0 z1 y1 material,
                XZRectHittable $ XZRect x0 x1 z0 z1 y0 material,
                YZRectHittable $ YZRect y0 y1 z0 z1 x1 material,
                YZRectHittable $ YZRect y0 y1 z0 z1 x0 material
            ] 
        in Box minPoint maxPoint listOfHittables 


makeBoundingBoxFromBox (Box minPoint maxPoint _) fromInterval toInterval result= 
    Right $ AABB minPoint maxPoint
