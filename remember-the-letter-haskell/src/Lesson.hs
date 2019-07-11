module Lesson
    ( lessonSummary
    ) where

flashcards :: [String]
flashcards = [flashcard1, flashcard2]



lessonSummary :: String
lessonSummary = foldr (++) "" (map showFlashcard flashcards)

showFlashcard :: String -> String
showFlashcard x = x ++ " \n "

flashcard1 :: String
flashcard1 = "the | le/la"

flashcard2 :: String
flashcard2 = "a | un/une"
