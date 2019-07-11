module Lesson
    ( lessonSummary
    ) where

data Flashcard =
  Flashcard
  { front :: String
  , back  :: String
  }

flashcards :: [Flashcard]
flashcards = [flashcard1, flashcard2]



lessonSummary :: String
lessonSummary = unlines (map showFlashcard flashcards)

showFlashcard :: Flashcard -> String
showFlashcard card = (front card) ++ " | " ++ (back card)

flashcard1 :: Flashcard
flashcard1 = Flashcard {front = "the", back = "le/la"}

flashcard2 :: Flashcard
flashcard2 = Flashcard {front = "a", back = "un/une"}
