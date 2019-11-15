module Fractals where

import Complex
import Data.List

projection :: Complex -> (Complex -> Complex) -> [Complex]
projection z func = z : projection (func z) func

comparison :: [Complex] -> Float -> [Bool]
comparison proj threshold = map (\z -> modulo z < threshold) proj

limited :: [Bool] -> Int -> [Bool]
limited bs n = take n bs

limit :: Complex -> (Complex -> Complex) -> Float -> Int -> Maybe Int
limit z func threshold n = elemIndex False $ limited (comparison (projection z func) threshold) n

