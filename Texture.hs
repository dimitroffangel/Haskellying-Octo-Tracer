module Texture where

import Vector
import PerlinShade

data Texture = 
    SolidColourTexture (SolidColour) 
    | 
    NoiseTexture (Noise) |
    SimpleTexture {
        oddTexture :: Texture,
        evenTexture :: Texture  
    } 
    | 
    EmptyTexture deriving (Show, Read, Eq)

newtype SolidColour = SolidColour {textureColour :: Vector} deriving (Show, Read, Eq)
newtype Noise = Noise {perlinShader :: PerlinShader} deriving (Show, Read, Eq)

-- only for test currently
getTextureValue (SolidColourTexture (SolidColour textureColour)) u v point = textureColour
getTextureValue (NoiseTexture (Noise perlinShader)) u v point = (Vector 1 1 1) 
getTextureValue (SimpleTexture oddTexture evenTexture) u v point@(Vector x y z) = 
    let sinusResult = (sin (10 * x)) * (sin (10 * y)) * (sin (10 * z))
    in if sinusResult < 0 
        then getTextureValue oddTexture u v point
        else getTextureValue evenTexture u v point
