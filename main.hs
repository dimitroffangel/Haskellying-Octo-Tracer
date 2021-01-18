module Task2 where

import Vector
import IO


widthTestImage :: Int
widthTestImage = 256
heightTestImage :: Int
heightTestImage = 256

testingPicture currentWidth currentHeight result
    | currentWidth == widthTestImage && currentHeight == 0 = Image widthTestImage heightTestImage $ splitListOnLists widthTestImage $ reverse result
    | currentWidth == widthTestImage = testingPicture 0 (currentHeight - 1) result
    | otherwise = 
        testingPicture (currentWidth + 1) currentHeight 
            $ Vector (realToFrac (255.999 * ((realToFrac currentWidth) / realToFrac (widthTestImage - 1))))
                  (realToFrac (255.999 * ((realToFrac currentHeight) / realToFrac (heightTestImage - 1))))
                  (realToFrac (0.25 * 255.999)) : result