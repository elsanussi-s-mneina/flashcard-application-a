{-|
Module      : LearnerLessonTests
Description : Automated tests of the LearnerLesson module belong here.
-}

module LearnerLessonTests (learnerLessonSpecs) where

import Prelude (($), (++) )

import Test.Hspec (describe, it, shouldBe, Spec)
import LearnerLesson 
  (LearnerFlashcard(LearnerFlashcard), learnerAnswersCorrectly)

import Lesson (Flashcard(Flashcard), front, back)

flashcard1 :: Flashcard
flashcard1 = Flashcard {front = "the", back = "le/la"}


learnerLessonSpecs :: Spec
learnerLessonSpecs =
  do
  describe "learnerAnswersCorrectly" $ do
    it ("should return the with a greater number of correct answers (greater by 1) "
        ++ " and one greater number of total attempts ") $ do
      learnerAnswersCorrectly (LearnerFlashcard flashcard1 0 2) `shouldBe` (LearnerFlashcard flashcard1 1 3)
      learnerAnswersCorrectly (LearnerFlashcard flashcard1 1 4) `shouldBe` (LearnerFlashcard flashcard1 2 5)
      learnerAnswersCorrectly (LearnerFlashcard flashcard1 35 40) `shouldBe` (LearnerFlashcard flashcard1 36 41)


