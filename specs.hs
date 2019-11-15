import Test.Hspec
import Test.QuickCheck
import Complex
import Fractals

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
      it "return true false" $ do
        take 4 (comparison (projection (Complex 0 0) (add (Complex 1 1))) 4 ) `shouldBe` [True, True, False, False]

    describe "limit" $ do
      it "return limit" $ do
        limit (Complex 0 0) (add (Complex 1 1)) 20 100 `shouldBe` Just 4
        limit (Complex 0 0) (add (Complex 0 0)) 20 100 `shouldBe` Nothing 