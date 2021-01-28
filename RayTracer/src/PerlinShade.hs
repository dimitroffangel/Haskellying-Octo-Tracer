module PerlinShade where

import Vector
import UtilityFunctions
import GeneratingRandomStuff

import Data.Bits

data PerlinShader = PerlinShader{
    randomUnitVectos :: [Vector],
    randomNumbers :: [Double],
    permutateX :: [Int],
    permutateY :: [Int],
    permutateZ :: [Int]
} deriving (Show, Read, Eq)

constructPerlinShader listOfRandomUnitVectors listOfRandomNumbers listOfRandomsForX listOfRandomsForY listOfRandomsForZ = 
    PerlinShader                    listOfRandomUnitVectors 
                                    listOfRandomNumbers 
                                    (perlinGeneratePermutation listOfRandomsForX) 
                                    (perlinGeneratePermutation listOfRandomsForY) 
                                    (perlinGeneratePermutation listOfRandomsForZ)

makePerlinNoise (PerlinShader _ randomNumbers permX permY permZ) point@(Vector x y z) = 
    let 
        i = ((.&.) (ceiling (4 * x)) 255) :: Int
        j = ((.&.) (ceiling (4 * y)) 255) :: Int
        k = ((.&.) (ceiling (4 * z)) 255) :: Int
        in randomNumbers !! ((permX !! i) `xor` (permY !! j) `xor` (permZ !! k))

-- createTurbulence (PerlinShader randomNumbers permX permY permZ) point@(Vector x y z) = []

trilinearInterpolationXSize = 2
trilinearInterpolationYSize = 2
trilinearInterpolationZSize = 2

makePerlinNoiseWithTrilnearInterpolation (PerlinShader randomUnitVectors randomNumbers permX permY permZ) point@(Vector x y z) =
    let 
        u = x - realToFrac (floor x)
        v = y - realToFrac (floor y)
        w = z - realToFrac (floor z)
        i = floor x
        j = floor y
        k = floor z
        dataForTrilinearInterpolation = 
            [
                [
                    [
                        randomUnitVectors !! (  (permX !! (.&.) (i + additionX) 255) `xor`
                                            (permY !! (.&.) (j + additionY) 255) `xor` 
                                            (permZ !! (.&.) (k + additionZ) 255)) | additionZ <- [0..trilinearInterpolationZSize]
                    ]
                    | additionY <- [0..trilinearInterpolationYSize]
                ] | additionX <- [0..trilinearInterpolationXSize]
            ]
        in trilinearInterpolation dataForTrilinearInterpolation u v w


trilinearInterpolation list u v w =
    trilinearInterpolationHelper 0 0 0 list 0
    where 
        hermitianU = u*u*(3 - 2*u)
        hermitianV = v*v*(3 - 2*v)
        hermitianW = w*w*(3 - 2*w)
        trilinearInterpolationHelper xIndex yIndex zIndex list result
            | zIndex == trilinearInterpolationZSize = trilinearInterpolationHelper xIndex (yIndex + 1) 0 list result
            | yIndex == trilinearInterpolationYSize = trilinearInterpolationHelper (xIndex + 1) 0 0 list result
            | xIndex == trilinearInterpolationXSize = result
            | otherwise = 
                let weightVector = 
                        Vector (u - realToFrac xIndex) (v - realToFrac yIndex) (w - realToFrac zIndex) 
                in trilinearInterpolationHelper xIndex yIndex (zIndex + 1) list 
                $ result + (dotProduct (list !! xIndex !! yIndex !! zIndex) weightVector * 
                    ((realToFrac xIndex * hermitianU + (1- realToFrac xIndex) * (1-hermitianU)) * 
                    (realToFrac yIndex * hermitianV + (1- realToFrac yIndex) * (1-hermitianV))  * 
                    (realToFrac zIndex * hermitianW + (1- realToFrac zIndex) * (1-hermitianW))))


pointCounter = 256

perlinGeneratePermutation listOfRandoms = 
    let array = [x | x <- [0 .. (pointCounter - 1)]]
    in perlinPermutate (pointCounter - 1) listOfRandoms array 

perlinPermutate _ [] result = result
perlinPermutate index (currentRandomNumber : restOfRandoms) result =
            let 
                temp = result !! index
                target = currentRandomNumber
                newResult = (take (index - 1) result) ++ (target : (drop index result))
                finalResult = (take (target - 1) newResult) ++ (temp : (drop target newResult))
                in perlinPermutate (index - 1)  restOfRandoms finalResult
