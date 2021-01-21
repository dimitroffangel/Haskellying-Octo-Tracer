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

getRay u v camera = 
    Ray (cameraOrigin camera) $ (cameraLowerLeftCorner camera) + scalarMultiplication (cameraHorizontal camera) u + 
                     (scalarMultiplication (cameraVertical camera) v - (cameraOrigin camera))

data Camera = Camera {
    cameraOrigin :: Vector,
    cameraLowerLeftCorner :: Vector, 
    cameraHorizontal:: Vector,
    cameraVertical :: Vector,
    cameraTheta :: Double, 
    cameraHeight :: Double,
    cameraViewportHeight :: Double,
    cameraViewportWidth :: Double,
    cameraFocalLength :: Double
} deriving (Show, Read, Eq)

constructCamera verticalFieldOfView aspectRatio =
    let theta = degreesToRadians verticalFieldOfView
        height = tan(theta / 2)
        viewportHeight = 2.0 * height
        viewportWidth = aspectRatio * viewportHeight
        focalLength = 1
        originLocation = Vector 0 0 0
        horizontal = Vector viewportWidth 0 0
        vertical = Vector 0 viewportHeight 0
        in Camera
            (Vector 0 0 0)
            (originLocation - scalarDivision horizontal 2 - scalarDivision vertical 2 - Vector 0 0 focalLength)
            horizontal
            vertical
            (degreesToRadians verticalFieldOfView)
            height
            viewportHeight
            viewportWidth
            focalLength
