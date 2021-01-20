module GeneratingRandomStuff where

import System.Random

generateNumberInInterval from to = do fst . randomR (from::Double, to::Double) <$> newStdGen