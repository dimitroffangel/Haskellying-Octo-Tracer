module Camera where

import Vector
import Ray
import UtilityFunctions

-- image info
aspectRatio = 16.0 / 9.0
imageWidth :: Int
imageWidth = 400
imageHeight :: Int
imageHeight = floor $ realToFrac imageWidth / aspectRatio

samplePerPixel = 10           

-- use of Right hand coordiante system
-- camera stuff
defaultViewportHeight = 2
defaultViewportWidth = ceiling $ aspectRatio * realToFrac defaultViewportHeight
-- distance between projection plane and the projection point
defaultFocalLength = 1.0

-- eye coordinate
defaultOriginLocation = Vector 0 0 0
defaultHorizontal = Vector (realToFrac defaultViewportWidth) 0 0
defaultVertical = Vector 0 2 0
defaultLowerLeftCorner = defaultOriginLocation - scalarDivision defaultHorizontal 2 - scalarDivision defaultVertical 2 - Vector 0 0 defaultFocalLength

getRay s t camera = 
    Ray (cameraOrigin camera) $ (cameraLowerLeftCorner camera) + scalarMultiplication (cameraHorizontal camera) s + scalarMultiplication (cameraVertical camera) t - (cameraOrigin camera)

data Camera = Camera {
    cameraOrigin :: Vector,
    cameraLowerLeftCorner :: Vector, 
    cameraHorizontal:: Vector,
    cameraVertical :: Vector,
    cameraTheta :: Double, 
    cameraHeight :: Double,
    cameraViewportHeight :: Double,
    cameraViewportWidth :: Double,
    cameraRelativeX :: Vector,
    cameraRelativeY :: Vector,
    cameraRelativeZ :: Vector 
} deriving (Show, Read, Eq)

constructCamera verticalFieldOfView aspectRatio lookFrom lookAt viewUp=
    let theta = degreesToRadians verticalFieldOfView
        height = tan(theta / 2)
        viewportHeight = 2.0 * height
        viewportWidth = aspectRatio * viewportHeight
        -- camera local orthonormal basis when camera faces z -> relative camera points at w
        w = getUnitVector (lookFrom - lookAt)
        u = getUnitVector (crossProduct viewUp w)
        v = crossProduct w u
        horizontal = scalarMultiplication u viewportWidth
        vertical = scalarMultiplication v viewportHeight
            in Camera
                lookFrom -- origin
                (lookFrom - scalarDivision horizontal 2 - scalarDivision vertical 2 - w) -- lowerleft
                horizontal 
                vertical
                (degreesToRadians verticalFieldOfView) -- theta
                height
                viewportHeight
                viewportWidth
                u
                v
                w
