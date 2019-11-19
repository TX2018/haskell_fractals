import Test.Hspec
import Test.QuickCheck
import Complex
import Fractals
import Conversion

main :: IO ()
main = hspec $ do
  describe "Complex" $ do
    describe "square" $ do
      it "return square of a complex number" $ do
        square (Complex 1 1)  `shouldBe` (Complex 0 2)

    describe "mul" $ do
      it "return product of two complex numbers" $ do
        mul (Complex 3 2) (Complex 1 7)  `shouldBe` (Complex (-11) 23)
        (Complex 3 2) `mul` (Complex 1 7)  `shouldBe` (Complex (-11) 23)

    describe "add" $ do
      it "return sum of two complex numbers" $ do
        add (Complex 3 2) (Complex 1 7)  `shouldBe` (Complex 4 9)

    describe "modulo" $ do
      it "return module of a complex number" $ do
        modulo (Complex 2 6) `shouldBe` 40
    
    -- describe "subtract" $ do
    --   it "return difference of two complex numbers" $ do
    --     subtract (Complex 4 8) (Complex 3 1)  `shouldBe` (Complex 1 7)

    describe "show" $ do
      it "prints complex number" $ do
        show (Complex 1 7)  `shouldBe` "1.0 + 7.0*i"

  describe "Fractals" $ do
    describe "projection" $ do
      it "creates an infinite array" $ do
        take 3 (projection (Complex 0 0) (add (Complex 1 1) )) `shouldBe` [(Complex 0 0), (Complex 1 1), (Complex 2 2)]

    describe "comparison" $ do
      it "return true false" $
        let result = take 4 $
                      comparison 
                        (projection (Complex 0 0) (add (Complex 1 1))) 
                        4 
            expected = [True, True, False, False]
        in result `shouldBe` expected 

    describe "limit" $ do
      it "return limit" $ do
        limit (Complex 0 0) (add (Complex 1 1)) 20 100 `shouldBe` Just 4
        limit (Complex 0 0) (add (Complex 0 0)) 20 100 `shouldBe` Nothing 

  
  describe "Conversion" $ do
    describe "cell_x" $ do
      it "calculates the width of the pixel" $ 
        let conversion = Conversion {
            screen_width = 10,
            screen_height = 20,
            origin_x = -2,
            origin_y = 2,
            real_width = 4,
            real_height = 4
        }
        in cell_x conversion `shouldBe` 0.4


    -- describe "real_x" $ do
    --   it "returns the real coor of the pixel" $
    --     do 
    --       (real_x conversion 0 `shouldBe` (-2.0))
    --       ((real_x conversion 9) - 1.6 < 0.001 `shouldBe` True)
    --       ((real_x conversion 4) + 0.4 < 0.001 `shouldBe` True)
    --       where 
    --         conversion = Conversion {
    --           screen_width = 10,
    --           screen_height = 20,
    --           origin_x = -2,
    --           origin_y = 2,
    --           real_width = 4,
    --           real_height = 4 
    --         } 
        
    describe "screen_to_real" $ do
      it "returns the complex number for screen coords" $
        do 
          ((screen_to_real conversion (0, 0)) `shouldBe` (Complex (-2) 2))
          where 
            conversion = Conversion {
              screen_width = 10,
              screen_height = 20,
              origin_x = -2,
              origin_y = 2,
              real_width = 4,
              real_height = 4
            }



