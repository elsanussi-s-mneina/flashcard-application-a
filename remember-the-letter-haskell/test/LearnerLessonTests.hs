{-|
Module      : LearnerLessonTests
Description : Automated tests of the LearnerLesson module belong here.
-}

module LearnerLessonTests (learnerLessonSpecs) where

import Prelude (($))

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
    it ("should return the with a greater score (greater by 1) ") $ do
      learnerAnswersCorrectly (LearnerFlashcard flashcard1 0) `shouldBe` (LearnerFlashcard flashcard1 1)
      learnerAnswersCorrectly (LearnerFlashcard flashcard1 1) `shouldBe` (LearnerFlashcard flashcard1 2)
      learnerAnswersCorrectly (LearnerFlashcard flashcard1 35) `shouldBe` (LearnerFlashcard flashcard1 36)
