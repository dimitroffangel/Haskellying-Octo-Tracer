module YZRect where

import AxisAlignedBoundingBox
import HitRecord
import Ray
import Vector
import HittableTypes


yzRectBoundingBox (YZRect y0 y1 z0 z1 x _) fromInterval toInterval result =
    Right $ AABB (Vector (x - thinBorderDistance) y0 z0) $ Vector (x - thinBorderDistance) y1 z1 

hitYZRect (YZRect y0 y1 z0 z1 yzRectX yzRectMaterial) 
 ray@(Ray rayOrigin rayDirection _) tMin tMax hitRecord@(HitRecord hitRecordPoint hitRecordNormal hitRecordMaterial u v hitRecordT hitRecordFrontFace) =
    let t = (yzRectX - (x rayOrigin)) / (x rayDirection)
        in if t < tMin || t > tMax 
            then Left hitRecord
            else 
                let 
                    intersectedY = (y rayOrigin) + t * (y rayDirection)
                    intersectedZ = (z rayOrigin) + t * (z rayDirection)
                    in if intersectedY  < y0 || intersectedY  > y1 || intersectedZ < z0 || intersectedZ > z1
                        then Left hitRecord
                        else 
                            let 
                                newU = (intersectedY - y0) / (y1 - y0)
                                newV = (intersectedZ - z0) / (z1 - z0)
                                newT = t
                                in Right $ 
                                    setFaceNormal (HitRecord (getPointLocation ray t) hitRecordNormal yzRectMaterial newU newV newT hitRecordFrontFace)
                                    ray $ Vector 0 0 1
