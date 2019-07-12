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

  describe "showFlashcardBack" $ do
    it ("should return \"five\" when the back side is \"five\"") $ do
      showFlashcardBack (Flashcard "" "five") `shouldBe` "five"
    it ("should return an empty string when the back side is empty") $ do
      showFlashcardBack (Flashcard "wh" "") `shouldBe` ""

  describe "tabSeparatedValuesOfFlashcard" $ do
    it ("should return tab then new line when the front side and back " ++
        "side are empty") $ do
      tabSeparatedValuesOfFlashcard (Flashcard "" "") `shouldBe` "\t\n"
    it ("should return \"turtle\" then tab then new line when " ++
        "the front side is \"turtle\" and the back side is empty") $ do
      tabSeparatedValuesOfFlashcard (Flashcard "turtle" "")
      `shouldBe`
      "turtle\t\n"
    it ("should return \"turtle\" then tab then \"fox\" then new line " ++
        "when the front side is \"turtle\" and the back side is \"fox\".") $ do
      tabSeparatedValuesOfFlashcard (Flashcard "turtle" "fox")
      `shouldBe`
      "turtle\tfox\n"

  describe "tabSeparatedValuesToFlashcard" $ do
    it ("should return a flashcard with turtle on the front side " ++
        "and fox on the back side " ++
        "when given turtle then a tab then fox then a new line.") $ do
          (tabSeparatedValuesToFlashcard "turtle\tfox\n")
            `shouldBe`
            (Flashcard "turtle" "fox")
    it ("should return a flashcard with rabbit on the front side " ++
        "and snail on the back side " ++
        "when given rabbit then a tab then snail then a new line") $ do
          (showFlashcard $  (tabSeparatedValuesToFlashcard "rabbit\tsnail\n"))
            `shouldBe`
            "rabbit | snail"
    it ("should not crash when given rabbit then a new line, " ++
        "it should just set the back to blank, " ++
        "and the front to \"rabbit\".") $ do
          (showFlashcard (tabSeparatedValuesToFlashcard "rabbit\n"))
            `shouldBe`
            "rabbit | "
  describe "tabSeparatedValuesToLesson" $ do
    it ("should give an empty list when given an empty string") $ do
      (tabSeparatedValuesToLesson "") `shouldBe` []
    it ("should give an 3 element list when given 3 lines") $ do
       length (tabSeparatedValuesToLesson "a\tb\nc\td\ne\tf\n")
       `shouldBe` 3
  describe "tabSeparatedValuesOfLesson" $ do
    it ("should return an empty string when given an empty list") $ do
      tabSeparatedValuesOfLesson [] `shouldBe` ""
    it ("should return three lines when given a 3-element list") $ do
      tabSeparatedValuesOfLesson [ (Flashcard "a"   "1")
                                 , (Flashcard "ab"  "2")
                                 , (Flashcard "abc" "3")]
      `shouldBe` "a\t1\nab\t2\nabc\t3\n"
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

  describe "backSummary" $ do
    it ("should return an empty string when given an empty list") $ do
      backSummary [] `shouldBe` ""
    it ("should return red followed by a new line followed by " ++
        "blue when given a list with two flashcards: the first one " ++
        "with back side equal to red, " ++
        "the second one with back side equal to blue") $ do
      backSummary [(Flashcard "a" "red"),
                    (Flashcard "b" "blue")]
      `shouldBe`
      "red\nblue\n"
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
