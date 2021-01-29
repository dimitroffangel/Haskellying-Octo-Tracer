module XYRect where

import AxisAlignedBoundingBox
import HitRecord
import Ray
import Vector
import HittableTypes


xzRectBoundingBox (XZRect x0 x1 z0 z1 y _) fromInterval toInterval result =
    Right $ AABB (Vector x0 (y - thinBorderDistance) z0) $ Vector x1 (y + thinBorderDistance) z1 

hitXZRect (XZRect x0 x1 z0 z1 xyRectY xzRectMaterial) 
 ray@(Ray rayOrigin rayDirection _) tMin tMax hitRecord@(HitRecord hitRecordPoint hitRecordNormal hitRecordMaterial u v hitRecordT hitRecordFrontFace) =
    let t = (xyRectY - (y rayOrigin)) / (y rayDirection)
        in if t < tMin || t > tMax 
            then Left hitRecord
            else 
                let 
                    intersectedX = (x rayOrigin) + t * (x rayDirection)
                    intersectedZ = (z rayOrigin) + t * (z rayDirection)
                    in if intersectedX  < x0 || intersectedX  > x1 || intersectedZ < z0 || intersectedZ > z1
                        then Left hitRecord
                        else 
                            let 
                                newU = (intersectedX - x0) / (x1 - x0)
                                newV = (intersectedY - z0) / (z1 - z0)
                                newT = t
                                in Right $ 
                                    setFaceNormal (HitRecord (getPointLocation ray t) hitRecordNormal xyRectMaterial newU newV newT hitRecordFrontFace)
                                    ray $ Vector 0 0 1
