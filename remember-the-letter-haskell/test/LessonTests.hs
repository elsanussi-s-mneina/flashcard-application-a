{-|
Module      : LessonTests
Description : Automated tests of the Lesson module belong here.
-}

module LessonTests (lessonSpecs) where

import Test.Hspec
import Lesson

flashcards :: [Flashcard]
flashcards = [flashcard1, flashcard2]
flashcard1 :: Flashcard
flashcard1 = Flashcard {front = "the", back = "le/la"}
flashcard2 :: Flashcard
flashcard2 = Flashcard {front = "a", back = "un/une"}


lessonSpecs :: Spec
lessonSpecs =
  do
  describe "showFlashcard" $ do
    it ("should return the front side " ++
        "then a pipe character then the back side") $ do
      showFlashcard (Flashcard "Z" "d") `shouldBe` "Z | d"
      showFlashcard (Flashcard "AB" "abc") `shouldBe` "AB | abc"
  describe "lessonSummary" $ do
    it ("should,return the front side then a pipe character " ++
        " then the back side of the first card " ++
        " followed by a new line followed by the front side " ++
        " then a pipe character" ++
        " then the back side of second card") $ do
        lessonSummary flashcards `shouldBe` "the | le/la\na | un/une\n"
    it "should return three lines when given a list of three cards" $ do
       lessonSummary [(Flashcard "a" "A"),
                      (Flashcard "b" "B"),
                      (Flashcard "c" "C")]
       `shouldBe`
        "a | A\nb | B\nc | C\n"
