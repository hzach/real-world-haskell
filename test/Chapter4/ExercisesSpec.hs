
module Chapter4.ExercisesSpec where
  import Chapter4.Exercises

  import Test.Hspec
  import Test.QuickCheck

  import Control.Exception

  spec :: Spec
  spec =
    describe "asInt_fold" $ do
      
      context "with good input" $
        it "returns Just Int" $ do
          asInt_fold "1337" `shouldBe` 1337
          asInt_fold "-420" `shouldBe` -420

      context "with bad input" $
        it "throws an exception" $ do
          evaluate (asInt_fold "") `shouldThrow` anyErrorCall
          evaluate (asInt_fold "4lph<a>Num3ric!") `shouldThrow` anyErrorCall
