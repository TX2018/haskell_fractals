module Fractals where

import Complex
import Data.List
import Conversion

projection :: Complex -> (Complex -> Complex) -> [Complex]
projection z func = z : projection (func z) func

comparison :: [Complex] -> Float -> [Bool]
comparison proj threshold = map (\z -> modulo z < threshold) proj

limited :: [Bool] -> Int -> [Bool]
limited bs n = take n bs

limit :: Complex -> (Complex -> Complex) -> Float -> Int -> Maybe Int
limit z func threshold n = elemIndex False $ limited (comparison (projection z func) threshold) n

lim_to_char :: Maybe Int -> Char
lim_to_char lim = case lim of
    Nothing -> ' '
    Just _ -> '-'

fractal :: (Complex -> Complex) -> Conversion -> Float -> [Char]
fractal func conv threshold = map lim_to_char limits where 
    dots = [(x, y)| x <- [0..(screen_width conv)], y <- [0..(screen_height conv)] ] 
    real_dots = map (screen_to_real conv) dots 
    limits = map (\z -> limit z func threshold 0) real_dots




