module Complex where

data Complex = Complex Float Float deriving (Eq)

square :: Complex -> Complex
square z = mul z z 

add :: Complex -> Complex -> Complex
add (Complex r1 i1) (Complex r2 i2) = Complex (r1+r2) (i1+i2)

-- subtract :: Complex -> Complex -> Complex
-- subtract (Complex r1 i1) (Complex r2 i2) = Complex (r1-r2) (i1-i2)

mul :: Complex -> Complex -> Complex
mul (Complex r1 i1) (Complex r2 i2) = Complex (r1*r2-i1*i2) (r1*i2+r2*i1)

instance Show Complex where
    show (Complex a b) = show a ++ " + " ++ show b ++ "*i"

modulo :: Complex -> Float
modulo (Complex re im) = re*re + im*im


-- instance Num Complex where
--    a + b = add a b 
--    a * b = mul a b 
--    a - b = subtract a b
--    abs    (Complex a b) = Complex (abs a) (abs b) 
--    signum (Complex a b) = Complex (signum a) (signum b)
--    fromInteger i = Complex (fromInteger i) (fromInteger i)