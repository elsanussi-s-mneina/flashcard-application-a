{-|
Module      : LessonTests
Description : Automated tests of the Lesson module belong here.
-}

module LessonTests (lessonSpecs) where

import Prelude (($), (++), length)

import Test.Hspec (describe, it, shouldBe, Spec)
import Lesson (addFlashcard, backSummary,
               frontSummary, summary,
               toTabSeparatedValues,
               fromTabSeparatedValues)
import Flashcard(Flashcard(Flashcard))
import qualified Flashcard (presentBack, presentFront, toTabSeparatedValues,
       fromTabSeparatedValues, showBack, showFront, show, front, back)

flashcards :: [Flashcard]
flashcards = [flashcard1, flashcard2]
flashcard1 :: Flashcard
flashcard1 = Flashcard {Flashcard.front = "the", Flashcard.back = "le/la"}
flashcard2 :: Flashcard
flashcard2 = Flashcard {Flashcard.front = "a", Flashcard.back = "un/une"}


lessonSpecs :: Spec
lessonSpecs =
  do
  describe "show" $ do
    it ("should return the front side " ++
        "then a pipe character then the back side") $ do
      Flashcard.show (Flashcard "Z" "d") `shouldBe` "Z | d"
      Flashcard.show (Flashcard "AB" "abc") `shouldBe` "AB | abc"
  describe "showFront" $ do
     it ("should return \"one\" when the front side is \"one\"") $ do
       Flashcard.showFront (Flashcard "one" "une") `shouldBe` "one"
     it ("should return \"three\" when the front side is \"three\"") $ do
       Flashcard.showFront (Flashcard "three" "h") `shouldBe` "three"

  describe "showBack" $ do
    it ("should return \"five\" when the back side is \"five\"") $ do
      Flashcard.showBack (Flashcard "" "five") `shouldBe` "five"
    it ("should return an empty string when the back side is empty") $ do
      Flashcard.showBack (Flashcard "wh" "") `shouldBe` ""

  describe "tabSeparatedValuesOfFlashcard" $ do
    it ("should return tab then new line when the front side and back " ++
        "side are empty") $ do
      Flashcard.toTabSeparatedValues (Flashcard "" "") `shouldBe` "\t\n"
    it ("should return \"turtle\" then tab then new line when " ++
        "the front side is \"turtle\" and the back side is empty") $ do
      Flashcard.toTabSeparatedValues (Flashcard "turtle" "")
      `shouldBe`
      "turtle\t\n"
    it ("should return \"turtle\" then tab then \"fox\" then new line " ++
        "when the front side is \"turtle\" and the back side is \"fox\".") $ do
      Flashcard.toTabSeparatedValues (Flashcard "turtle" "fox")
      `shouldBe`
      "turtle\tfox\n"

  describe "tabSeparatedValuesToFlashcard" $ do
    it ("should return a flashcard with turtle on the front side " ++
        "and fox on the back side " ++
        "when given turtle then a tab then fox then a new line.") $ do
          (Flashcard.fromTabSeparatedValues "turtle\tfox\n")
            `shouldBe`
            (Flashcard "turtle" "fox")
    it ("should return a flashcard with rabbit on the front side " ++
        "and snail on the back side " ++
        "when given rabbit then a tab then snail then a new line") $ do
          (Flashcard.show $  (Flashcard.fromTabSeparatedValues "rabbit\tsnail\n"))
            `shouldBe`
            "rabbit | snail"
    it ("should not crash when given rabbit then a new line, " ++
        "it should just set the back to blank, " ++
        "and the front to \"rabbit\".") $ do
          (Flashcard.show (Flashcard.fromTabSeparatedValues "rabbit\n"))
            `shouldBe`
            "rabbit | "
  describe "Lesson.fromTabSeparatedValues" $ do
    it ("should give an empty list when given an empty string") $ do
      (Lesson.fromTabSeparatedValues "") `shouldBe` []
    it ("should give an 3 element list when given 3 lines") $ do
       length (Lesson.fromTabSeparatedValues "a\tb\nc\td\ne\tf\n")
       `shouldBe` 3
  describe "Lesson.toTabSeparatedValues" $ do
    it ("should return an empty string when given an empty list") $ do
      Lesson.toTabSeparatedValues [] `shouldBe` ""
    it ("should return three lines when given a 3-element list") $ do
      Lesson.toTabSeparatedValues [ (Flashcard "a"   "1")
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
        "the second one with front side \"b\"") $ do
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
  describe "Lesson.summary" $ do
    it ("should,return the front side then a pipe character " ++
        " then the back side of the first card " ++
        " followed by a new line followed by the front side " ++
        " then a pipe character" ++
        " then the back side of second card") $ do
        Lesson.summary flashcards `shouldBe` "the | le/la\na | un/une\n"
    it "should return three lines when given a list of three cards" $ do
       Lesson.summary [(Flashcard "a" "A"),
                       (Flashcard "b" "B"),
                       (Flashcard "c" "C")]
       `shouldBe`
        "a | A\nb | B\nc | C\n"
  describe "addFlashcard" $ do
    it "should return a list with one more flashcard" $ do
      length $ addFlashcard [(Flashcard "a" "b")] "c" "d"
      `shouldBe` 2
    it ("should return a list with the flashcard" ++
        " at the end of the original list") $ do
      Lesson.addFlashcard [(Flashcard "x" "y"), (Flashcard "b" "c")]
                            "r" "s"
      `shouldBe`           [(Flashcard "x" "y"),
                            (Flashcard "b" "c"),
                            (Flashcard "r" "s")]
  describe "presentBack" $ do
    it "should show the back of a flashcard in context" $ do
      Flashcard.presentBack (Flashcard "f23" "1234b") `shouldBe` "I am showing you the back of a flashcard.\nYou see \"1234b\""
      Flashcard.presentBack (Flashcard "zzz" "321n8 s") `shouldBe` "I am showing you the back of a flashcard.\nYou see \"321n8 s\""
    it "should show the front of a flashcard in context" $ do
      Flashcard.presentFront (Flashcard "f23" "1234b") `shouldBe` "I am showing you the front of a flashcard.\nYou see \"f23\""
      Flashcard.presentFront (Flashcard "zzz" "321n8 s") `shouldBe` "I am showing you the front of a flashcard.\nYou see \"zzz\""
