module IO where

import Vector

data Image = Image { width   :: Int,
                    height  :: Int,
                    content :: [[Rgb]] } deriving (Show,Read, Eq)


splitListOnLists _ [] = []
splitListOnLists numberOfPartsToSplitTheList list = first : splitListOnLists numberOfPartsToSplitTheList rest
    where 
        (first, rest) = splitAt numberOfPartsToSplitTheList list

convertIntToString 0 = "0"
convertIntToString number =
    convertIntToStringHelper number []
    where 
        convertIntToStringHelper number result 
            | number == 0 = result
            | otherwise = convertIntToStringHelper (number `div` 10) (intToDigit (number `mod` 10) : result)  

convertRgbToString (Rgb red green blue) = 
    convertIntToString (fromIntegral red) ++ " " ++
    convertIntToString (fromIntegral green) ++ " " ++ convertIntToString (fromIntegral blue) ++ "\n"

saveImage (Image imageWidth imageHeight imageContent) filePath = writeFile 
        -- convert each Rgb object to string, in the newly formed array of array of string, concat the arrays in one array of string
        filePath $ "P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255\n" ++ concat (concatMap (map convertRgbToString) imageContent)

convertStringToImage [] currentNumber True result = reverse $ currentNumber : result
convertStringToImage [] _ False result = reverse result
convertStringToImage (head : rest) currentNumber isReadingNumber result
    -- if has stopped reading nubmer, add the number and continue onward
    | isReadingNumber && (not . isDigit) head = convertStringToImage rest 0 False $ currentNumber : result
    -- read the digit of the current number
    | isDigit head  = convertStringToImage rest (currentNumber * 10 + digitToInt head) True result
    | otherwise = convertStringToImage rest 0 False result

-- if it does not read the format will return the data for emptyImage
readFormat [] _ _ = "0 0 255"
readFormat stringInput [] _ = stringInput 
readFormat (headInput : restOfInput) format@(headFormat : restOfFormat) isReadingFormat
    | headInput == ' ' && not isReadingFormat = readFormat restOfInput format  False
    | headInput == headFormat = readFormat restOfInput restOfFormat True
    | otherwise = readFormat restOfInput format isReadingFormat

expectedFileFormat = "P3"

loadImage filePath = do  
    handle <- openFile filePath ReadMode  
    string <- hGetContents handle
    let 
        integers = convertStringToImage (readFormat (readFormat string expectedFileFormat False) "\n" False) 0 False []
        width = head integers
        height = head . tail $ integers
        -- create the Rgb objects from the integers in one array -> split the array on height number of arrays each with length width
        -- create the final image from width height and the newly formed content from
        in return $ Image width height $ splitListOnLists width $ bindPixels (drop 3 integers) []

bindPixels :: [Int] -> [Rgb] -> [Rgb]
bindPixels [] result = reverse result
bindPixels (red : green : blue : tail) result = bindPixels tail 
    (Rgb (fromIntegral red::Word8) (fromIntegral green::Word8) (fromIntegral blue::Word8) : result)

