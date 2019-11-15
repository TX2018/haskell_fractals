import Test.Hspec
import Test.QuickCheck
import Complex

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

    -- describe "subtract" $ do
    --   it "return difference of two complex numbers" $ do
    --     subtract (Complex 4 8) (Complex 3 1)  `shouldBe` (Complex 1 7)

    describe "show" $ do
      it "prints complex number" $ do
        show (Complex 1 7)  `shouldBe` "1.0 + 7.0*i"


