import Vector
import IO
import AxisAlignedBoundingBox
import UtilityFunctions

import Test.HUnit

threeOnTwoPicture = wrapInIO $ Image 3 2 [ 
                    [Vector 255 0 0, Vector 155 128 0, Vector 255 0 0], 
                    [Vector 255 0 0, Vector 255 0 0, Vector 128 255 128]
                 ]    
                 

emptyPicture = wrapInIO $ Image 0 0 []

generateRgbArrayWithOneVariety :: (Eq a, Num a) => Rgb -> a -> [Rgb] -> [Rgb]
generateRgbArrayWithOneVariety _ 0 result = result
generateRgbArrayWithOneVariety rgb@(Vector r g b) length result = generateRgbArrayWithOneVariety rgb (length - 1) (rgb : result)

cloneListIntoListOfItselfTimes :: (Eq t, Num t) => a -> t -> [a] -> [a]
cloneListIntoListOfItselfTimes _ 0 result = result 
cloneListIntoListOfItselfTimes list length result = cloneListIntoListOfItselfTimes list (length - 1) (list : result) 

testingVectorStuff :: Test
testingVectorStuff = TestCase $ do
    assertEqual "Summing vector with identy" (Vector 3 3 3) $ Vector 3 3 3 + Vector 0 0 0
    assertEqual "Normal Summing vector" (Vector 5 10 15) $ Vector 4 8 13 + Vector 1 2 2
    assertEqual "Product of zero vector and random other" (Vector 0 0 0) $ Vector 0 0 0 * Vector 1 1 1    
    assertEqual "Scalar multiplication with zero scalar" (Vector 0 0 0 ) $ scalarMultiplication (Vector 3 42 6) 0
    assertEqual "Scalar division get identy" (Vector 3 3 3) $ scalarDivision (Vector 12 12 12) 4 
    assertEqual "Scalar double division" (Vector 2.5 7.5 15) $ scalarDivision (Vector 5 15 30) 2
    assertEqual "Normal test Scalar multiplication" (Vector 12 52 120) $ scalarMultiplication (Vector 3 13 30) 4
    assertEqual "DotProduct of a vector and a zero vector" 0 $ dotProduct (Vector 0 0 0) (Vector 1 2 3)
    assertEqual "Normal test DotProduct"  38 $ dotProduct (Vector 2 3 4) (Vector 3 4 5)
    assertEqual "UnitVector remains the same" (Vector 1 0 0) $ getUnitVector (Vector 1 0 0)
    assertEqual "CrossProduct of linear dependent vectors" (Vector 0 0 0) $ crossProduct (Vector 2 2 4) (Vector 4 4 8) 
    assertEqual "Normal test Cross product" (Vector (-3) 6 (-3)) $ crossProduct (Vector 2 3 4) (Vector 5 6 7)


compareFunc x y = x < y 

testingUtilityFunctions :: Test
testingUtilityFunctions = TestCase $ do
    assertEqual "Quicksort empty list" ([]::[Int]) $ quicksort ([]::[Int]) compareFunc
    assertEqual "Normal quicksort" [1,2,3,4,5,6] $ quicksort [3,4,2, 1,5,6] compareFunc
    assertEqual "reverse list quickoset" [1,2,3,4,5,6] $ quicksort [6,5,4, 3,2,1] compareFunc
    assertEqual "stationary list" [1,2,3,4] $ quicksort [1,2,3,4] compareFunc



testingSurroundingBoxAABB :: Test
testingSurroundingBoxAABB = TestCase $ do
    assertEqual "2d AABB" (AABB (Vector 1 1 0) (Vector 6 6 0)) $ makeSurroundingBox (AABB (Vector 1 1 0) (Vector 2 2 0)) (AABB (Vector 3 3 0) (Vector 6 6 0))  
    assertEqual "3d AABB" (AABB (Vector 3 6 2) (Vector 19 21 1765)) $ 
        makeSurroundingBox (AABB (Vector 3 15 2) (Vector 19 21 5)) (AABB (Vector 3 6 42) (Vector 6 6 1765))  
    assertEqual "2d inverted interval" (AABB (Vector 1 1 0) (Vector 3 3 5)) $ 
        makeSurroundingBox (AABB (Vector 6 6 0) (Vector 2 2 0)) (AABB (Vector 1 1 0) (Vector 3 3 5))
    -- normally this would not happen so it is left like that otherwise would be wrapped in a monad
    assertEqual "2d inverted interval both" (AABB (Vector 6 6 0) (Vector 3 3 5)) $ 
        makeSurroundingBox (AABB (Vector 6 6 0) (Vector 2 2 0)) (AABB (Vector 6 6 10) (Vector 3 3 5))


tests :: Test
tests =
    TestList
    [
        testingVectorStuff,
        testingUtilityFunctions,
        testingSurroundingBoxAABB
    ]

main :: IO Counts
main = runTestTT tests