module Conversion where

import Complex

data Conversion = Conversion {
    screen_width :: Int,
    screen_height :: Int,
    origin_x :: Float,
    origin_y :: Float,
    real_width :: Float,
    real_height :: Float
}

cell_x :: Conversion -> Float
cell_x conv = (real_width conv) / (fromIntegral (screen_width conv))

cell_y :: Conversion -> Float
cell_y conv = (real_height conv) / (fromIntegral (screen_height conv))

real_x :: Conversion -> Int -> Float
real_x conv x = (origin_x conv) + (fromIntegral x) * (cell_x conv)

real_y :: Conversion -> Int -> Float
real_y conv y = (origin_y conv) + (fromIntegral y) * (cell_y conv)

screen_to_real :: Conversion -> (Int, Int) -> Complex
screen_to_real conv (x, y) = Complex (real_x conv x) (real_y conv y)



