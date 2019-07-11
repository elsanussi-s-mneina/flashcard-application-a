module Lesson
    ( lessonSummary
    ) where

data Flashcard = Flashcard String String

flashcards :: [Flashcard]
flashcards = [flashcard1, flashcard2]



lessonSummary :: String
lessonSummary = unlines (map showFlashcard flashcards)

showFlashcard :: Flashcard -> String
showFlashcard (Flashcard a b) = a ++ " | " ++ b

flashcard1 :: Flashcard
flashcard1 = (Flashcard "the" "le/la")

flashcard2 :: Flashcard
flashcard2 = (Flashcard "a" "un/une")
