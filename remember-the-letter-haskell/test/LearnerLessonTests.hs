{-|
Module      : LearnerLessonTests
Description : Automated tests of the LearnerLesson module belong here.
-}

module LearnerLessonTests (learnerLessonSpecs) where

import Prelude (($), (++) )

import Test.Hspec (describe, it, shouldBe, Spec)
import Types.LearnerFlashcard(LearnerFlashcard(LearnerFlashcard))
import LearnerLesson 
  (learnerAnswersCorrectly, learnerAnswersIncorrectly)

import Flashcard (Flashcard(Flashcard))
import qualified Flashcard (front, back)

flashcard1 :: Flashcard
flashcard1 = Flashcard {Flashcard.front = "the", Flashcard.back = "le/la"}


learnerLessonSpecs :: Spec
learnerLessonSpecs =
  do
  describe "learnerAnswersCorrectly" $ do
    it ("should return the flashcard with a greater number of correct answers (greater by 1) "
        ++ "and one greater number of total attempts ") $ do
      learnerAnswersCorrectly (LearnerFlashcard flashcard1 0 2) `shouldBe` (LearnerFlashcard flashcard1 1 3)
      learnerAnswersCorrectly (LearnerFlashcard flashcard1 1 4) `shouldBe` (LearnerFlashcard flashcard1 2 5)
      learnerAnswersCorrectly (LearnerFlashcard flashcard1 35 40) `shouldBe` (LearnerFlashcard flashcard1 36 41)
  describe "learnerAnswersIncorrectly" $ do
    it ("should return the flashcard with the same number of correct answers, " 
        ++ "but with a number of attempts greater by 1.") $ do
      learnerAnswersIncorrectly (LearnerFlashcard flashcard1 0 2) `shouldBe` (LearnerFlashcard flashcard1 0 3)
      learnerAnswersIncorrectly (LearnerFlashcard flashcard1 9 18) `shouldBe` (LearnerFlashcard flashcard1 9 19)
      learnerAnswersIncorrectly (LearnerFlashcard flashcard1 83 100) `shouldBe` (LearnerFlashcard flashcard1 83 101)

