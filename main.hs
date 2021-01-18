module Task2 where

import System.IO  
import Data.Char (intToDigit, isDigit, digitToInt)

import Vector
import IO


widthTestImage :: Int
widthTestImage = 256
heightTestImage :: Int
heightTestImage = 256

testingPicture currentWidth currentHeight result
    | currentWidth == 0 && currentHeight == -1 = Image widthTestImage heightTestImage $ splitListOnLists widthTestImage result
    | currentWidth == 0 = testingPicture (widthTestImage - 1) (currentHeight - 1) result
    | otherwise = 
        testingPicture (currentWidth - 1) currentHeight 
            $ Rgb (floor (255.999 * (realToFrac currentWidth / realToFrac (widthTestImage - 1))))
                  (floor (255.999 * (realToFrac currentHeight / realToFrac (heightTestImage - 1))))
                  (floor (0.25 * 255.999)) : result