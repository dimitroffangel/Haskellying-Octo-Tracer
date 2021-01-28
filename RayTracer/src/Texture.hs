module Texture where

import Vector
import PerlinShade

data Texture = 
    SolidColourTexture SolidColour 
    | 
    NoiseTexture Noise 
    |
    TrilinearNoiseTexture TrilinearNoise 
    | 
    MarbleTexture TrilinearNoise
    |
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
getTextureValue (SimpleTexture oddTexture evenTexture) u v point@(Vector x y z) = 
    let sinusResult = (sin (10 * x)) * (sin (10 * y)) * (sin (10 * z))
    in if sinusResult < 0 
        then getTextureValue oddTexture u v point
        else getTextureValue evenTexture u v point
