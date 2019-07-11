module Main where

import Lib (welcome)
import Lesson

flashcards :: [Flashcard]
flashcards = [flashcard1, flashcard2]

flashcard1 :: Flashcard
flashcard1 = Flashcard {front = "the", back = "le/la"}

flashcard2 :: Flashcard
flashcard2 = Flashcard {front = "a", back = "un/une"}


main :: IO ()
main =
  do
  welcome
  putStrLn ""
  putStrLn "Printing Lesson summary:"
  putStrLn (lessonSummary flashcards)
