
module Chapter4.ExercisesSpec where
  import Chapter4.Exercises

  import Test.Hspec
  import Test.QuickCheck

  import Control.Exception

  spec :: Spec
  spec =
    describe "asIntFold" $ do

      context "with good input" $
        it "returns an Int" $ do
          asIntFold "1337" `shouldBe` 1337
          asIntFold "-420" `shouldBe` -420

      context "with bad input" $
        it "throws an exception" $ do
          evaluate (asIntFold "") `shouldThrow` anyErrorCall
          evaluate (asIntFold "-") `shouldThrow` anyErrorCall
          evaluate (asIntFold "4lph<a>Num3ric!") `shouldThrow` anyErrorCall
