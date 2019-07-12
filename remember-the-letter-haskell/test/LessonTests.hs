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
  describe "showFlashcardFront" $ do
     it ("should return \"one\" when the front side is \"one\"") $ do
       showFlashcardFront (Flashcard "one" "une") `shouldBe` "one"
     it ("should return \"three\" when the front side is \"three\"") $ do
       showFlashcardFront (Flashcard "three" "h") `shouldBe` "three"
  describe "frontSummary" $ do
    it ("should return an empty string when given an empty list") $ do
      frontSummary [] `shouldBe` ""
    it ("should return \"one\" followed by a new line when given a " ++
        "list with a single flashcard whose front side is \"one\" ") $ do
      frontSummary [(Flashcard "one" "une")] `shouldBe` "one\n"
    it ("should return \"a\" followed by a new line " ++
        "followed by a \"b\" followed by a new line when given a " ++
        "list with a two flashcards the first one with front side is \"a\" " ++
        "the second one with fornt side \"b\"") $ do
      frontSummary [(Flashcard "a" "sf"), (Flashcard "b" "wh")]
      `shouldBe` "a\nb\n"

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
