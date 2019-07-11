module Lesson where

data Flashcard =
  Flashcard
  { front :: String
  , back  :: String
  }




lessonSummary :: [Flashcard] -> String
lessonSummary flashcards = unlines (map showFlashcard flashcards)

showFlashcard :: Flashcard -> String
showFlashcard card = (front card) ++ " | " ++ (back card)
