module PerlinShade where

import Vector
import UtilityFunctions
import GeneratingRandomStuff

import Data.Bits

data PerlinShader = PerlinShader{
    randomNumbers :: [Double],
    permutateX :: [Int],
    permutateY :: [Int],
    permutateZ :: [Int]
} deriving (Show, Read, Eq)

constructPerlinShader listOfRandomNumbers listOfRandomsForX listOfRandomsForY listOfRandomsForZ = 
    PerlinShader listOfRandomNumbers (perlinGeneratePermutation listOfRandomsForX) 
                                    (perlinGeneratePermutation listOfRandomsForY) 
                                    (perlinGeneratePermutation listOfRandomsForZ)

makePerlinNoise (PerlinShader randomNumbers permX permY permZ) point@(Vector x y z) = 
    let 
        i = ((.&.) (ceiling (4 * x)) 255) :: Int
        j = ((.&.) (ceiling (4 * y)) 255) :: Int
        k = ((.&.) (ceiling (4 * z)) 255) :: Int
        in randomNumbers !! ((permX !! i) `xor` (permY !! j) `xor` (permZ !! k))

trilinearInterpolationXSize = 2
trilinearInterpolationYSize = 2
trilinearInterpolationZSize = 2

makePerlinNoiseWithTrilnearInterpolation (PerlinShader randomNumbers permX permY permZ) point@(Vector x y z) =
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
                         randomNumbers !! (  (permX !! (.&.) (i + c) 255) `xor`
                                            (permY !! (.&.) (j + b) 255) `xor` 
                                            (permZ !! (.&.) (k + a) 255)) | a <- [0..trilinearInterpolationXSize]
                    ]
                    | b <- [0..trilinearInterpolationYSize]
                ] | c <- [0..trilinearInterpolationZSize]
            ]
        in trilinearInterpolation dataForTrilinearInterpolation u v w


trilinearInterpolation list u v w =
    trilinearInterpolationHelper 0 0 0 list u v w 0
    where 
        trilinearInterpolationHelper zIndex yIndex xIndex list u v w result
            | xIndex == trilinearInterpolationXSize = trilinearInterpolationHelper zIndex (yIndex + 1) 0 list u v w result
            | yIndex == trilinearInterpolationYSize = trilinearInterpolationHelper (zIndex + 1) 0 0 list u v w result
            | zIndex == trilinearInterpolationZSize = result
            | otherwise = trilinearInterpolationHelper zIndex yIndex (xIndex + 1) list u v w 
                $ result + 
                    (realToFrac zIndex *u + (1- realToFrac zIndex)*(1-u)) * 
                    (realToFrac yIndex *v+ (1- realToFrac yIndex)*(1-v)) * 
                    (realToFrac xIndex *w+ (1- realToFrac xIndex)*(1-w)) * (((list !! zIndex) !! yIndex) !! xIndex) 


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
