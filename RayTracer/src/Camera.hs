module Camera where

import Vector
import Ray
import UtilityFunctions

-- image info
aspectRatio = 16 / 9
imageWidth :: Int
imageWidth = 600
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

getRay s t camera unitDiskVector randomInitialStartOfCasting = 
    let 
        radiusVector@(Vector xRadiusVector yRadiusVector zRadiusVector) = scalarMultiplication unitDiskVector (cameraLensRadius camera)
        offset = scalarMultiplication (cameraRelativeU camera) xRadiusVector + scalarMultiplication (cameraRelativeV camera) yRadiusVector
    in  Ray (cameraOrigin camera + offset)
        ((cameraLowerLeftCorner camera) + scalarMultiplication (cameraHorizontal camera) s + scalarMultiplication (cameraVertical camera) t - 
        (cameraOrigin camera) - offset)
        $ randomInitialStartOfCasting

data Camera = Camera {
    cameraOrigin :: Vector,
    cameraLowerLeftCorner :: Vector, 
    cameraHorizontal:: Vector,
    cameraVertical :: Vector,
    cameraTheta :: Double, 
    cameraHeight :: Double,
    cameraViewportHeight :: Double,
    cameraViewportWidth :: Double,
    cameraRelativeU :: Vector,
    cameraRelativeV :: Vector,
    cameraRelativeW :: Vector,
    cameraLensRadius :: Double,
    sendRayFromTime :: Double,
    sendRayUntilTime :: Double
} deriving (Show, Read, Eq)


-- aperture -> tool to control the lense with wich the depth of field is made
-- focusDist -> the distance from the lense to the projection plane
constructCamera verticalFieldOfView aspectRatio aperture focusDist lookFrom lookAt viewUp sendRayFromTime sendRayUntilTime =
    let theta = degreesToRadians verticalFieldOfView
        height = tan(theta / 2)
        viewportHeight = 2.0 * height
        viewportWidth = aspectRatio * viewportHeight
        -- camera local orthonormal basis when camera faces z -> relative camera points at w
        w = getUnitVector (lookFrom - lookAt)
        -- u,v and viewUp are in one plane
        -- u,v,w camera relative coordinate space
        u = getUnitVector (crossProduct viewUp w)
        v = crossProduct w u
        horizontal = scalarMultiplication u (viewportWidth * focusDist)
        vertical = scalarMultiplication v (viewportHeight * focusDist)
            in Camera
                lookFrom -- origin
                (lookFrom - scalarDivision horizontal 2 - scalarDivision vertical 2 - scalarMultiplication w focusDist) -- lowerleft
                horizontal 
                vertical
                (degreesToRadians verticalFieldOfView) -- theta
                height
                viewportHeight
                viewportWidth
                u
                v
                w
                (aperture / 2)
                sendRayFromTime
                sendRayUntilTime