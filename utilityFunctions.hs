module UtilityFunctions where

-- utility stuff
degreesToRadians degrees = (degrees * pi) / 180

clamp x min max 
    | x < min = min
    | x > max = max
    | otherwise = x