module LessonTests (lessonSpecs) where

import Test.Hspec
import Lesson (lessonSummary)

lessonSpecs :: Spec
lessonSpecs =
  describe "lessonSummary" $ do
    it "should return exactly the value" $ do
      lessonSummary `shouldBe` "the | le/la \n a | un/une\n"
