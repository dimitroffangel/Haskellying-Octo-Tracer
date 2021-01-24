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

pointCounter = 256

perlinGeneratePermutation listOfRandoms = 
    let array = [x | x <- [0 .. (pointCounter - 1)]]
    in perlinPermutate (pointCounter - 1) array listOfRandoms

perlinPermutate index (currentRandomNumber : restOfRandoms) result
    | index == -1 = result
    | otherwise = 
            let 
                temp = result !! index
                target = result !! currentRandomNumber
                newResult = (take (index - 1) result) ++ (target : (drop index result))
                finalResult = (take (target - 1) newResult) ++ (temp : (drop target newResult))
                in perlinPermutate (index - 1) finalResult restOfRandoms
