module ConstantMedium where

import HittableTypes
import HitRecord
import Texture

constructConstantMedium hittable density colour = 
    ConstantMedium hittable (IsotropicMaterial $ SolidColourTexture $ SolidColour colour) (1/density)