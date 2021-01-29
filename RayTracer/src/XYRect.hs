module XYRect where

import AxisAlignedBoundingBox
import HitRecord
import Ray
import Vector
import HittableTypes


xyRectBoundingBox (XYRect x0 x1 y0 y1 z _) fromInterval toInterval result =
    Right $ AABB (Vector x0 y0 $ z - thinBorderDistance) $ Vector x1 y1 $ z + thinBorderDistance

hitXYRect (XYRect x0 x1 y0 y1 xyRect xyRectMaterial) 
 ray@(Ray rayOrigin rayDirection _) tMin tMax hitRecord@(HitRecord hitRecordPoint hitRecordNormal hitRecordMaterial u v hitRecordT hitRecordFrontFace) =
    let t = (xyRect - (z rayOrigin)) / (z rayDirection)
        in if t < tMin || t > tMax 
            then Left hitRecord
            else 
                let 
                    intersectedX = (x rayOrigin) + t * (x rayDirection)
                    intersectedY = (y rayOrigin) + t * (y rayDirection)
                    in if intersectedX  < x0 || intersectedX  > x1 || intersectedY < y0 || intersectedY > y1
                        then Left hitRecord
                        else 
                            let 
                                newU = (intersectedX - x0) / (x1 - x0)
                                newV = (intersectedY - y0) / (y1 - y0)
                                newT = t
                                in Right $ 
                                    setFaceNormal (HitRecord (getPointLocation ray t) hitRecordNormal xyRectMaterial newU newV newT hitRecordFrontFace)
                                    ray $ Vector 0 0 1
