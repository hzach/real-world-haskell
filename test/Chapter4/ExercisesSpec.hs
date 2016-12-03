
module Chapter4.ExercisesSpec where
  import Chapter4.Exercises

  import Test.Hspec
  import Test.QuickCheck

  import Control.Exception

  spec :: Spec
  spec = do
    describe "asIntFold" $ do
      context "with good input" $
        it "returns an Int" $ do
          asIntFold "1337" `shouldBe` 1337
          asIntFold "-420" `shouldBe` -420
      context "with bad input" $
        it "throws an exception" $ do
          evaluate (asIntFold "")                `shouldThrow` anyErrorCall
          evaluate (asIntFold "-")               `shouldThrow` anyErrorCall
          evaluate (asIntFold "4lph<a>Num3ric!") `shouldThrow` anyErrorCall

    describe "asIntEither" $ do
      context "with good input" $
        it "returns an Int" $ do
          asIntEither "0"    `shouldBe` Right 0
          asIntEither "1"    `shouldBe` Right 1
          asIntEither "1337" `shouldBe` Right 1337
          asIntEither "-420" `shouldBe` Right (-420)
      context "with bad input" $
        it "returns an ErrorMessage" $ do
          asIntEither ""  `shouldBe` Left "Empty String"
          asIntEither "-" `shouldBe` Left "Invalid"
          asIntEither "4o4" `shouldBe` Left "Not a digit 'o'"
