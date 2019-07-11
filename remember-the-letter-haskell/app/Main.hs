module Main where

import Lib (welcome)
import Lesson

-- | list of flashcards for running the program
flashcards :: [Flashcard]
flashcards = [flashcard1, flashcard2]

flashcard1 :: Flashcard
flashcard1 = Flashcard {front = "the", back = "le/la"}

flashcard2 :: Flashcard
flashcard2 = Flashcard {front = "a", back = "un/une"}

-- | This program runs in the terminal. It outputs
-- text to the student.
main :: IO ()
main =
  do
  welcome     -- show a welcome message.
  putStrLn "" -- blank line
  putStrLn "Printing Lesson summary:"
  putStrLn (lessonSummary flashcards)
