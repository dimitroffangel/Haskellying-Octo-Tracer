module UtilityFunctions where

-- utility stuff
degreesToRadians degrees = (degrees * pi) / 180

clamp x min max 
    | x < min = min
    | x > max = max
    | otherwise = x


wrapInIO :: a -> IO a
wrapInIO = return


quicksort [] _ = []
quicksort (xHead : xTail) compareFunc = 
    quicksort lesser compareFunc ++ (xHead : quicksort greater compareFunc)
    where 
            lesser = filter (not . compareFunc xHead) xTail
            greater = filter (compareFunc xHead) xTail