module Texture where

import Vector
import PerlinShade
import UtilityFunctions

data Texture = 
    SolidColourTexture SolidColour 
    | 
    NoiseTexture Noise 
    |
    TrilinearNoiseTexture TrilinearNoise 
    | 
    MarbleTexture TrilinearNoise
    |
    ImageTexture {
        imageData :: [[Vector]],
        widthImage :: Int,
        heightImage :: Int
    }|
    SimpleTexture {
        oddTexture :: Texture,
        evenTexture :: Texture  
    } 
    | 
    EmptyTexture deriving (Show, Read, Eq)

newtype SolidColour = SolidColour {textureColour :: Vector} deriving (Show, Read, Eq)
newtype Noise = Noise {perlinShader :: PerlinShader} deriving (Show, Read, Eq)
data TrilinearNoise = TrilinearNoise {
    trilinearPerlinShader :: PerlinShader,
    scale :: Double
} deriving (Show, Read, Eq)

-- only for test currently
getTextureValue (SolidColourTexture (SolidColour textureColour)) u v point = textureColour
getTextureValue (NoiseTexture (Noise perlinShader)) u v point = scalarMultiplication (Vector 1 1 1) (makePerlinNoise perlinShader point)
getTextureValue (TrilinearNoiseTexture (TrilinearNoise perlinShader scale)) u v point = scalarMultiplication (Vector 1 1 1) 
    $ createTurbulence perlinShader (scalarMultiplication point scale) 7
getTextureValue (MarbleTexture (TrilinearNoise perlinShader scale)) u v point@(Vector x y z) = 
    scalarMultiplication (Vector 1 1 1) $ 0.5 * (1 + sin(scale * z  + 10 * createTurbulence perlinShader point 7 ))

getTextureValue imageTexture@(ImageTexture imageData imageWidth imageHeight) u v point = 
        calculateColour i j 
        where
            clampedU = clamp u 0 1 
            clampedV = 1 - clamp v 0 1 -- flip v to image coordinate
            i = floor $ clampedU * realToFrac imageWidth
            j = floor $ clampedV * realToFrac imageHeight
            colourScale = 1.0 / 255.0
            calculateColour i j
                | i >= imageWidth = calculateColour (imageWidth - 1) j
                | j >= imageHeight = calculateColour i $ imageHeight - 1
                | otherwise =  
                    let Vector x y z = imageData !! j !! i 
                    in Vector x y z
                    -- in Vector (colourScale * x) (colourScale * y) (colourScale * z)

getTextureValue (SimpleTexture oddTexture evenTexture) u v point@(Vector x y z) = 
    let sinusResult = (sin (10 * x)) * (sin (10 * y)) * (sin (10 * z))
    in if sinusResult < 0 
        then getTextureValue oddTexture u v point
        else getTextureValue evenTexture u v point
