module Ray where

import Data.Word

import Vector


data Ray = Ray {
    origin :: Vector,
    direction :: Vector 
} deriving (Show, Read, Eq)

getPointLocation (Ray origin direction) scalar= origin + scalar * direction  


